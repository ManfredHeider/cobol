      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.07.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3154.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3154.CPv  $
      *
      *   Rev 2.5   Sep 25 2017 16:50:42   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen  PGMs
      *
      *   Rev 2.4   Sep 25 2017 15:25:14   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen  PGMs
      *
      *   Rev 2.3   Sep 11 2017 11:43:10   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen  PGMs
      *
      *   Rev 2.2   Aug 28 2017 10:07:00   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.0   Jul 19 2017 14:45:14   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER zum Aufbereitungsprogramm für Aktualisierung    *
      *       Vertriebshinweise DeuBa                                  *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01533!01.08.17!1.0 ! Neu                         !WHO        *
      *        !        !    !                             !           *
      *        !        !    !                             !           *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI3154.
      *
               10  'XX'-GES                  PIC  X(002).
      *            GESELLSCHAFT
               10  'XX'-VNR.
                   20 'XX'-FILIALNUMMER      PIC  9(003).
                   20 'XX'-KDNR              PIC  9(007).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               10  'XX'-VNR-1-10     REDEFINES 'XX'-VNR PIC 9(10).

               10 'XX'-REST.
                   15 'XX'-LAUFZEIT         PIC S9(04) .
                   15 'XX'-TILGGRAD         PIC S9(04) .
                   15 'XX'-VERSPAK          PIC X(10) .
                   15 'XX'-SALDO            PIC S9(07)V99.
                   15 'XX'-EFFEKTIVZINS     PIC S9(02)V9999.
                   15 'XX'-NOMINALZINS      PIC S9(02)V9999.
                   15 'XX'-AUFSTOCKUNGH     PIC X(01) .
                   15 'XX'-HOCHKREDITAF     PIC X(01) .




