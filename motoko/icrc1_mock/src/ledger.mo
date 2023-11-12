import Array "mo:base/Array";
import AssocList "mo:base/AssocList";
import Blob "mo:base/Blob";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Logo "logo";

actor class ICRC1Ledger() = self {

  // Types
  type Account = { var balance : Nat; };
  public type Subaccount = Blob;
  public type AccountRefOpt = { owner : Principal; subaccount : ?Subaccount };
  public type AccountRef = { owner : Principal; subaccount : Subaccount };
  public type TransferArgs = {
    from_subaccount : ?Subaccount;
    to : AccountRefOpt;
    amount : Nat;
    fee : ?Nat;
    memo : ?Blob;
    created_at_time : ?Nat64;
  };
  public type TransferError = {
    #BadFee : { expected_fee : Nat };
    #BadBurn : { min_burn_amount : Nat };
    #InsufficientFunds : { balance : Nat };
    #TooOld;
    #CreatedInFuture : { ledger_time : Nat64 };
    #Duplicate : { duplicate_of : Nat };
    #TemporarilyUnavailable;
    #GenericError : { error_code : Nat; message : Text };
  };

  // Subaccounts
  let zeroSubaccount = Blob.fromArray(Array.tabulate<Nat8>(32, func(n) = 0));
  func deoptRef(r : AccountRefOpt) : AccountRef = ({
    owner = r.owner;
    subaccount = Option.get(r.subaccount, zeroSubaccount);
  });
  func accRefEqual(a : AccountRef, b : AccountRef) : Bool = Principal.equal(a.owner, b.owner) and Blob.equal(a.subaccount, b.subaccount);

  // Define a map to store accounts
  var accounts : AssocList.AssocList<AccountRef, Account> = null;
  private func getBalance(account : AccountRefOpt) : Nat = switch (AssocList.find<AccountRef, Account>(accounts, deoptRef(account), accRefEqual)) {
    case (null) 0;
    case (?acc) acc.balance;
  };

  // Parameters
  var fee : Nat = 10;
  let decimals : Nat = 2;
  let name : Text = "My toy token";
  let symbol : Text = "MYX";
  let logo = Logo.icrc1_grey;

  // ================ ICRC1 API updates ================
  public shared ({ caller }) func icrc1_transfer(args : TransferArgs) : async ({
    #Ok : Nat;
    #Err : TransferError;
  }) {
    switch (AssocList.find<AccountRef, Account>(accounts, { owner = caller; subaccount = Option.get(args.from_subaccount, zeroSubaccount) }, accRefEqual)) {
      case (null) #Err(#InsufficientFunds({ balance = 0 }));
      case (?fromAcc) {
        if (Option.get(args.fee, fee) != fee) {
          return #Err(#BadFee { expected_fee = fee });
        };
        if (fromAcc.balance < args.amount + fee) {
          return #Err(#InsufficientFunds({ balance = fromAcc.balance }));
        };
        fromAcc.balance -= args.amount + fee;
        var newBalance : Nat = args.amount;
        switch (AssocList.find(accounts, deoptRef(args.to), accRefEqual)) {
          case (null) {
            accounts := AssocList.replace<AccountRef, Account>(accounts, deoptRef(args.to), accRefEqual, ?{ var balance = args.amount }).0;
          };
          case (?toAcc) {
            toAcc.balance += args.amount;
            newBalance := toAcc.balance;
          };
        };
        #Ok(newBalance);
      };
    };
  };

  // ================ ICRC1 API queries ================
  public shared query func icrc1_balance_of(account : AccountRefOpt) : async Nat = async getBalance(account);
  public shared query func icrc1_fee() : async Nat = async fee;
  public shared query func icrc1_decimals() : async Nat = async 2;
  public shared query func icrc1_name() : async Text = async name;
  public shared query func icrc1_symbol() : async Text = async symbol;
  type Value = { #Nat : Nat; #Int : Int; #Text : Text; #Blob : Blob };
  public shared query func icrc1_metadata() : async [(Text, Value)] = async [
    ("icrc1:symbol", #Text symbol),
    ("icrc1:decimals", #Nat decimals),
    ("icrc1:name", #Text name),
    ("icrc1:logo", #Text logo),
  ];

  // =============================================== CUSTOM API ==============================================
  // Define a function to add some tokens to account for testing
  public shared func issueTokens(account : AccountRefOpt, tokensAmount : Nat) : async () {
    let curBalance = getBalance(account);
    accounts := AssocList.replace<AccountRef, Account>(accounts, deoptRef(account), accRefEqual, ?{ var balance = curBalance + tokensAmount }).0;
  };
  public shared func updateFee(newFee : Nat) : async () {
    fee := newFee;
  };
};
