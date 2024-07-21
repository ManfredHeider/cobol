      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM PMKI1375              *
      *     - AUSGABEDATEI Tabelle  VERTRAG                            *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *07.03.18! NEU                                  ! BST     !      *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------
      * DBO-10293  !26.09.22! NEUE FELDER DATVERTRAEND,    ! T13AC20
      *            !        !             VSNRZUSATZ       !
      * -----------!--------!------------------------------!----------
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *--------------------------------------------------------------- *
      *                                                                *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *     LAENGE: 140 BYTES                   STAND: 07.03.2018      *
      **************************************************************** *

       01  DRKI3306.

           05  'XX'-VERARB-KZ               PIC  X(001).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete
           05  'XX'-VERTRAGSID              PIC  X(013).
           05  'XX'-GES                     PIC  9(002).
           05  'XX'-VNR                     PIC  9(015).

           05  'XX'-VBNR                    PIC  9(007).

           05  'XX'-VERTRAGSBEGINN          PIC  X(010).
           05  'XX'-VERTRAGSBEGINN-AENDKZ   PIC  X(001).
     **       'J' wenn Vertragsbeginn geändert

           05  'XX'-VERTRAGSABLAUF          PIC  X(010).
           05  'XX'-VERTRAGSABLAUF-AENDKZ   PIC  X(001).
     **       'J' wenn Vertragsablauf geändert

           05  'XX'-VERTRAGSZUSTAND         PIC S9(002).
           05  'XX'-VERTRAGSZUSTAND-AENDKZ  PIC  X(001).
      *        'J' wenn Vertragszustand geändert

           05  'XX'-BEITRAG-GESAMT          PIC  9(009)V99.
           05  'XX'-BEITRAG-GESAMT-AENDKZ   PIC  X(001).
     **       'J' wenn Beitrag Gesamt geändert

           05  'XX'-ZAHLWEISE               PIC S9(002).
           05  'XX'-ZAHLWEISE-AENDKZ        PIC  X(001).
      *        'J' wenn Zahlweise geändert

           05  'XX'-ZUSTANDDAT              PIC  X(010).
           05  'XX'-ZUSTANDDAT-AENDKZ       PIC  X(001).
     **       'J' wenn Vertragszustand geändert

           05  'XX'-STORNO-DATUM            PIC  X(010).
           05  'XX'-STORNO-DATUM-AENDKZ     PIC  X(001).
      *        'J' wenn Stornodatum geändert

           05  'XX'-GEBUENDELTKZ            PIC  X(001).
           05  'XX'-GEBUENDELTKZ-AENDKZ     PIC  X(001).
      *        'J' wenn Gebündelt-Kz geändert

           05  'XX'-MANDANT                 PIC  9(007).

           05  'XX'-DATVERTRAEND            PIC  X(010).

           05  'XX'-VSNRZUSATZ              PIC  X(010).

           05  FILLER                       PIC  X(011).
      *---------------------------------------------------------------*

