      ****************************************************************
      *    COPY-MEMBER: DRAN0179                                     *
      *    FUNKTION   : UPDATE PIN IN POA-BESTAND                    *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00357!19.03.02!2.1 !NEU                        !HEIDER    *
      * AAN00529!26.11.04!2.2 !OPTIMIERUNG POA-QUOTE      !RAHNER    *
      *         !        !    !                           !          *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME    *
      * -------------------------------------------------------------*
      * ANTRAG-1881!12.01.23! ERWEITERN UPDATE DRAN0180    ! RAHNER  *
      *            !        !                              !         *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0179                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0179.
           05 PI-DRAN0179-POAA-KEY.
               10 PI-DRAN0179-PGEB-GES           PIC  X(002).
               10 PI-DRAN0179-POA-HVANTNUM       PIC  X(009).
          05 PI-DRAN0179-DATEN.
               10 PI-DRAN0179-POA-HAND           PIC  X(001).
120123         10 PI-DRAN0179-POA-PDVNBLZ-E      PIC S9(008) COMP.
120123         10 PI-DRAN0179-POA-PDVNKONTONR-E  PIC S9(010) COMP.
120123         10 PI-DRAN0179-POA-PDVN-IBAN-E    PIC  X(034).
120123         10 PI-DRAN0179-POA-PDVN-BIC-E     PIC  X(010).
120123         10 PI-DRAN0179-POA-PDVNBEGINKM-E  PIC S9(006) COMP.
120123         10 PI-DRAN0179-POA-PDVNFGESTNR-E  PIC  X(017).
120123         10 PI-DRAN0179-POA-PDVNAMTLKZ-E   PIC  X(011).
120123         10 PI-DRAN0179-POA-PDVNJAHRKM-E   PIC S9(006) COMP.
120123         10 PI-DRAN0179-POA-WIDERSPR-E     PIC  X(001).
      *
       01 PO-DRAN0179.
          05 PO-DRAN0179-RC                    PIC  X(003).
             88 PO-DRAN0179-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0179-RC-NOK                  VALUE 'NOK'.
      ******** ENDE COPY-MEMBER **************************************
