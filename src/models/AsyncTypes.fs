[<AutoOpen>]
module AsyncTypes

type 't Deferred =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type 't AsyncOperationStatus =
    | Started
    | Finished of 't
