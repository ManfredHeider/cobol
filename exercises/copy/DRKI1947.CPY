      *****************************************************************
      *    COPY-MEMBER: DRKI1947                                      *
      *                                                               *
      *   DTA ZAHLUNGEN                                               *
      *   GEDLEBEN - G20 (PSLIFE)                                     *
      *                                                               *
      *   SATZLÄNGE VARIABEL                                          *
      *                                                               *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DBO-8439   !26.04.21! NEU                          ! T13AC20  *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    HAUPTSATZ SA 'C'
           05 HAUPTSATZ.
              10 SA                              PIC  X(001).
              10 VNR                             PIC  X(015).
              10 GEVO                            PIC  X(020).
              10 DATUM                           PIC  X(010).
              10 BETRAG                          PIC S9(011)V99.
      *
      *    VORSATZ   SA 'A'
           05 VORSATZ  REDEFINES HAUPTSATZ.
              10 V-SA                            PIC  X(001).
              10 V-ABSENDER                      PIC  X(003).
              10 V-ERSTELLDAT                    PIC  X(010).
              10 V-EMPFAENGER                    PIC  X(004).
              10 V-INHALT                        PIC  X(020).
      *
      *    NACHSATZ  SA 'E'
           05 NACHSATZ REDEFINES HAUPTSATZ.
              10 N-SA                            PIC  X(001).
              10 N-ANZSATZ                       PIC  9(009).
      *****************************************************************
      *---------------------- ENDE DRKI1947 --------------------------* 
      *****************************************************************
