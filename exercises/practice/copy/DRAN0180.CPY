      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0180  FUNKTION UPDATE POA-HAND           *
      *                                    ÜBER VORSCHLAGSNUMMER     *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!01.12.06!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME    *
      * -------------------------------------------------------------*
      * ANTRAG-1881!12.01.23! ERWEITERN UPDATE DRAN0180    ! RAHNER  *
      *            !        !                              !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0180                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE POAA TBAN0003              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0180.
           05 PI-DRAN0180-POAA-KEY.
               10 PI-DRAN0180-PGEB-GES          PIC S9(004) COMP.
               10 PI-DRAN0180-POA-HVANTNUM      PIC  X(009).
           05 PI-DRAN0180-POAA-DATEN.
               10 PI-DRAN0180-POA-HAND          PIC  X(001).
120123         10 PI-DRAN0180-POA-PDVNBLZ-E     PIC S9(008) COMP.
120123         10 PI-DRAN0180-POA-PDVNKONTONR-E PIC S9(010) COMP.
120123         10 PI-DRAN0180-POA-PDVN-IBAN-E   PIC  X(034).
120123         10 PI-DRAN0180-POA-PDVN-BIC-E    PIC  X(010).
120123         10 PI-DRAN0180-POA-PDVNBEGINKM-E PIC S9(006) COMP.
120123         10 PI-DRAN0180-POA-PDVNFGESTNR-E PIC  X(017).
120123         10 PI-DRAN0180-POA-PDVNAMTLKZ-E  PIC  X(011). 
120123         10 PI-DRAN0180-POA-PDVNJAHRKM-E  PIC S9(006) COMP.
120123         10 PI-DRAN0180-POA-WIDERSPR-E    PIC  X(001).
      *
       01  PO-DRAN0180.
           05 PO-DRAN0180-RC                   PIC  X(003).
             88 PO-DRAN0180-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0180-RC-CHANGED              VALUE 'CHA'.
             88 PO-DRAN0180-RC-NOT-FOUND            VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
