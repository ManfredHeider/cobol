       01 Dsrunner-Info-Block.
         03 Dsrunner-Screenset-Id            Pic X(8).
         03 Dsrunner-Session-Id              Pic 9(9) Comp-5.
         03 Dsrunner-Screenset-Instance      Pic 9(2) Comp-5.
         03 Dsrunner-System-Error.
           05 Dsrunner-Error-Code            Pic 9(4) Comp-5.
           05 Dsrunner-Error-Details-1       Pic 9(4) Comp-5.
           05 Dsrunner-Error-Details-2       Pic 9(4) Comp-5.
         03 Dsrunner-Validation-Error-No     Pic 9(4) Comp-5.
      *  Below this point are new since DSRUNNER v 1.0.08
         03 Dsrunner-Shared-Memory           Pointer.
         03 Dsrunner-Reserved Occurs 4.
           05 Filler                         Pic X(4).
         03 Dsrunner-User-Area Occurs 4.
           05 Filler                         Pic X(4).
