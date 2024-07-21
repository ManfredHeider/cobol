      *****************************************************************
      *    COPY-MEMBER: DRVB1317                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LIST-OZP ZXVB0145                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: U1302134                                         *
      *    DATUM:    08.07.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  | AENDERUNG                      | NAME      *
      * --------+--------+--------------------------------+-----------*
      * VLA-1057|31.08.20| NEU                            | HITZEMANN *
      *         |        |                                |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1317 - LESEN BWBAS-EINTRÄGE                   *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1317.
          05 PI-DRVB1317-UFKT-CODE             PIC  9(02).
          05 PI-DRVB1317-DATEN.
             10 PI-DRVB1317-STATUS             PIC  X(01).
                88 PI-DRVB1317-INITIAL              VALUE 'I'.
             10 PI-DRVB1317-TYP                PIC  X(01).
                88 PI-DRVB1317-ALLE                 VALUE 'A'.
                88 PI-DRVB1317-STRUKW               VALUE 'S'.
                88 PI-DRVB1317-DIRW                 VALUE 'D'.

       01 PO-DRVB1317.
         03 PO-DRVB1317-RC                     PIC  X(03).
            88 PO-DRVB1317-RC-OK                    VALUE 'OK '.
            88 PO-DRVB1317-RC-NOT-FOUND             VALUE 'NFD'.
            88 PO-DRVB1317-RC-EOF                   VALUE 'EOF'.
         03 PO-DRVB1317-DATEN.
          05 PO-DRVB1317-NI-VIVB0145.             
             10 NI-DRVB1317-BWBAS-UNTST-NR     PIC S9(04)       BINARY.
             10 NI-DRVB1317-BWBAS-DAT-VON      PIC S9(04)       BINARY.
             10 NI-DRVB1317-BWBAS-MS-NR        PIC S9(04)       BINARY.
             10 NI-DRVB1317-BWBAS-DAT-ERSTELL  PIC S9(04)       BINARY.
             10 NI-DRVB1317-BWBAS-ZUSTRUK-TYP  PIC S9(04)       BINARY.
             10 NI-DRVB1317-BWBAS-HERKUNFT     PIC S9(04)       BINARY.
             10 NI-DRVB1317-BWBAS-BETR-NR      PIC S9(04)       BINARY.
             10 NI-DRVB1317-BWBAS-EBENE        PIC S9(04)       BINARY.
             10 NI-DRVB1317-BWBAS-DIR-NEU      PIC S9(04)       BINARY.
             10 NI-DRVB1317-BWBAS-DIR-ALT      PIC S9(04)       BINARY.
             10 NI-DRVB1317-BWBAS-STAT         PIC S9(04)       BINARY.
             10 NI-DRVB1317-BWBAS-LAST-UPDATE  PIC S9(04)       BINARY.
          05 PO-DRVB1317-DCL-VIVB0145.
             10 PO-DRVB1317-UNTST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1317-DAT-VON            PIC  X(10).
             10 PO-DRVB1317-MS-NR              PIC S9(04)       BINARY.
             10 PO-DRVB1317-DAT-ERSTELL        PIC  X(10).
             10 PO-DRVB1317-ZUSTRUK-TYP        PIC S9(04)       BINARY.
             10 PO-DRVB1317-HERKUNFT           PIC  X(01).
             10 PO-DRVB1317-BETR-NR            PIC S9(09)       BINARY.
             10 PO-DRVB1317-EBENE              PIC S9(04)       BINARY.
             10 PO-DRVB1317-DIR-NEU            PIC S9(04)       BINARY.
             10 PO-DRVB1317-DIR-ALT            PIC S9(04)       BINARY.
             10 PO-DRVB1317-STAT               PIC S9(04)       BINARY.
             10 PO-DRVB1317-LAST-UPDATE        PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1317   --------------------------*
      *---------------------------------------------------------------*