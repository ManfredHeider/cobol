      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0996.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0996.CPv  $
      *
      *   Rev 2.1   Aug 31 2010 10:14:42   t13ex13
      *AAS00834: RGT: AS231
      *
      *   Rev 2.0   Aug 26 2010 14:13:50   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0996                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE DATEI DIR-LISTE MAP                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    26.08.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00834!26.08.10!2.1 ! NEU                       !GUTFLEISCH *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************

       01 DL-MAP.
          05 TITEL1.
             10 SEITENVORSCHUB                 PIC  X(001) VALUE '1'.
             10 TIT1-TITEL                     PIC  X(024) VALUE
                'MAP FÜR DIREKTIONSLEITER'.
             10 FILLER                         PIC  X(085) VALUE SPACES.
             10 TIT1-DATUM-TITEL               PIC  X(006) VALUE
                'DATUM '.
             10 TIT1-DATUM                     PIC  X(010).
             10 FILLER                         PIC  X(007) VALUE SPACES.
          05 TITEL2.
             10 FILLER                         PIC  X(001) VALUE SPACES.
             10 TIT2-TITEL                     PIC  X(031) VALUE
                'NEUE FIKTIVE GRUPPENLEITERSTUFE'.
             10 FILLER                         PIC  X(078) VALUE SPACES.
             10 TIT2-LISTENNUMMER              PIC  X(023) VALUE
                'LISTE   QAS1781F       '.
          05 TITEL3.
             10 FILLER                         PIC  X(001) VALUE SPACES.
             10 TIT3-TITEL                     PIC  X(024) VALUE
                'AUSWERTUNG FÜR VERTEILER'.
             10 FILLER                         PIC  X(108) VALUE SPACES.
          05 UEBERSCHRIFT1.
             10 FILLER                         PIC  X(091) VALUE SPACES.
             10 UEB1-LEISTUNG                  PIC  X(042) VALUE
                'MAP-LEISTUNG IM 12 MONATSZEITRAUM *'.
          05 UEBERSCHRIFT2.
             10 FILLER                         PIC  X(002) VALUE SPACES.
             10 UEB2-DG                        PIC  X(012) VALUE
                'DG'.
             10 UEB2-DIR                       PIC  X(009) VALUE
                'DIR'.
             10 UEB2-VBNR                      PIC  X(006) VALUE
                'VBNR'.
             10 UEB2-NAME                      PIC  X(032) VALUE
                'NAME'.
             10 UEB2-VORNAME                   PIC  X(011) VALUE
                'VORNAME'.
             10 UEB2-STUFE                     PIC  X(007) VALUE
                'STUFE'.
             10 UEB2-BEFOERDERT                PIC  X(012) VALUE
                'BEFOERDERT'.
             10 UEB2-MON01                     PIC  X(003) VALUE '01 '.
             10 UEB2-MON02                     PIC  X(003) VALUE '02 '.
             10 UEB2-MON03                     PIC  X(003) VALUE '03 '.
             10 UEB2-MON04                     PIC  X(003) VALUE '04 '.
             10 UEB2-MON05                     PIC  X(003) VALUE '05 '.
             10 UEB2-MON06                     PIC  X(003) VALUE '06 '.
             10 UEB2-MON07                     PIC  X(003) VALUE '07 '.
             10 UEB2-MON08                     PIC  X(003) VALUE '08 '.
             10 UEB2-MON09                     PIC  X(003) VALUE '09 '.
             10 UEB2-MON10                     PIC  X(003) VALUE '10 '.
             10 UEB2-MON11                     PIC  X(003) VALUE '11 '.
             10 UEB2-MON12                     PIC  X(003) VALUE '12 '.
             10 FILLER                         PIC  X(007) VALUE SPACES.
          05 TRENNZEILE.
             10 FILLER                         PIC  X(002) VALUE SPACES.
             10 TRENN-DG                       PIC  X(011) VALUE
                '---------  '.
             10 TRENN-DIR                      PIC  X(006) VALUE
                '----  '.
             10 TRENN-VBNR                     PIC  X(010) VALUE
                '--------  '.
             10 TRENN-NAME                     PIC  X(032) VALUE
                '------------------------------  '.
             10 TRENN-VORNAME                  PIC  X(011) VALUE
                '---------  '.
             10 TRENN-STUFE                    PIC  X(007) VALUE
                '-----  '.
             10 TRENN-BEFOERDERT               PIC  X(012) VALUE
                '----------  '.
             10 TRENN-MON01                    PIC  X(003) VALUE '-- '.
             10 TRENN-MON02                    PIC  X(003) VALUE '-- '.
             10 TRENN-MON03                    PIC  X(003) VALUE '-- '.
             10 TRENN-MON04                    PIC  X(003) VALUE '-- '.
             10 TRENN-MON05                    PIC  X(003) VALUE '-- '.
             10 TRENN-MON06                    PIC  X(003) VALUE '-- '.
             10 TRENN-MON07                    PIC  X(003) VALUE '-- '.
             10 TRENN-MON08                    PIC  X(003) VALUE '-- '.
             10 TRENN-MON09                    PIC  X(003) VALUE '-- '.
             10 TRENN-MON10                    PIC  X(003) VALUE '-- '.
             10 TRENN-MON11                    PIC  X(003) VALUE '-- '.
             10 TRENN-MON12                    PIC  X(003) VALUE '-- '.
             10 FILLER                         PIC  X(007) VALUE SPACES.
          05 DATEN.
             10 FILLER                         PIC  X(002) VALUE SPACES.
             10 DATEN-DG                       PIC  X(009).
             10 FILLER                         PIC  X(002) VALUE SPACES.
             10 DATEN-DIR                      PIC  Z999.
             10 FILLER                         PIC  X(003) VALUE SPACES.
             10 DATEN-VBNR                     PIC  9999999.
             10 FILLER                         PIC  X(002) VALUE SPACES.
             10 DATEN-NAME                     PIC  X(030).
             10 FILLER                         PIC  X(002) VALUE SPACES.
             10 DATEN-VORNAME                  PIC  X(009).
             10 FILLER                         PIC  X(002) VALUE SPACES.
             10 DATEN-STUFE                    PIC  X(005).
             10 FILLER                         PIC  X(002) VALUE SPACES.
             10 DATEN-BEFOERDERT               PIC  X(010).
             10 FILLER                         PIC  X(002) VALUE SPACES.
             10 DATEN-MON01                    PIC  X(003).
             10 DATEN-MON02                    PIC  X(003).
             10 DATEN-MON03                    PIC  X(003).
             10 DATEN-MON04                    PIC  X(003).
             10 DATEN-MON05                    PIC  X(003).
             10 DATEN-MON06                    PIC  X(003).
             10 DATEN-MON07                    PIC  X(003).
             10 DATEN-MON08                    PIC  X(003).
             10 DATEN-MON09                    PIC  X(003).
             10 DATEN-MON10                    PIC  X(003).
             10 DATEN-MON11                    PIC  X(003).
             10 DATEN-MON12                    PIC  X(003).
             10 FILLER                         PIC  X(006) VALUE SPACES.
          05 LEERZEILE.
             10 FILLER                         PIC  X(133) VALUE SPACES.
          05 FUSS1.
             10 FILLER                         PIC  X(096) VALUE
                ' ERKLAERUNGEN ZUR MONATSUEBERSICHT DER MAP-LEISTUNGEN'.
             10 FILLER                         PIC  X(037) VALUE
                ' * DIE MAP-LEISTUNG BEGINNT IM'.
          05 FUSS2.
             10 FILLER                         PIC  X(096) VALUE
                ' J = BEDINGUNGEN ERFUELLT / MAP-LEISTUNG'.
             10 FILLER                         PIC  X(037) VALUE
                ' 1.MONAT NACH DER BEFOERDERUNG'.
          05 FUSS3.
             10 FILLER                         PIC  X(133) VALUE
                ' N = BEDINGUNGEN NICHT ERFUELLT / KEINE MAP-LEISTUNG'.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB0996   --------------------------*
      *---------------------------------------------------------------*
