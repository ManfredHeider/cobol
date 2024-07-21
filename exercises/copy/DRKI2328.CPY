      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2328.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2328.CPv  $
      *
      *   Rev 2.4   Mar 09 2016 14:44:22   t13ex73
      *AAT00049: Fehlerverfahren mit Prozentkriterien - G48 PGMs
      *
      *   Rev 2.3   Nov 02 2009 10:04:20   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.2   Oct 27 2009 12:37:48   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.1   Aug 17 2009 16:48:04   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.0   Aug 02 2009 14:13:32   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR ÄNDERUNGSBESTAND DEUTSCHE BANK DEPOTSTAMM   *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Länge = 100 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00844!01.08.09! 01 ! Neu                       !Hornung   *
      *--------------------------------------------------------------*
      * AAT00049!03.03.16! Umbau auf Replacing by XX      !DDY       *
      *         !        ! - ohne Vermerk in AREA A -     !          *
      *--------------------------------------------------------------*
      *         !        !                                !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI2328.
      *
           10  'XX'-SATZ.
      *
               20  'XX'-SA                 PIC  X(002).
      *            SATZART        = '17'
               20  'XX'-GES                PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  'XX'-VNR.
                   25  'XX'-FILIAL-NR      PIC 9(03).
                   25  'XX'-KDNR           PIC 9(07).
                   25  'XX'-VERT-ORDNUNGSNR PIC 9(02).
               20  FILLER   REDEFINES 'XX'-VNR.
                   25  'XX'-VNR-1          PIC 9(10).
                   25  FILLER             PIC X(02).
               20  'XX'-PORTFOLIO-NR       PIC X(10).
      *            PORTFOLIONMUMMER
               20  'XX'-DAT-DEPOT-EROEFF   PIC X(08).
      *            DATUM DEPOTERÖFFNUNG
               20  'XX'-DAT-DEPOT-SCHLIES  PIC X(08).
      *            DATUM DEPOTSCHLIEßUNG
               20  'XX'-DEL-KZ             PIC X(01).
      *            LÖSCH-KENNZEICHEN
               20  FILLER                 PIC X(057).
      *            RESERVE
      *
           10  'XX'-VORSATZ REDEFINES 'XX'-SATZ.
      *
               20  'XX'-V-SA           PIC X(002).
      *            SATZART = '00'
               20  'XX'-V-ABSENDER     PIC X(008).
      *            ABSENDER = 'DWS  '
               20  'XX'-V-DATUM        PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  'XX'-V-EMPFAENGER   PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  'XX'-V-TEXT         PIC X(008).
      *            INHALTSANGABE:
      *            DEPOTSTAMM       TÄGLICH   ŸSTAMMT  '
      *            DEPOTSTAMMN      KOMPLETT  ŸSTAMMK  '
               20  'XX'-V-WKZ          PIC X(003).
      *            Währungs-Kennz. =  'EUR'
               20  FILLER             PIC X(067).
      *            RESERVE
 
           10  'XX'-NACHSATZ REDEFINES 'XX'-SATZ.
      *
               20  'XX'-N-SA           PIC X(002).
      *            SATZART             '99'
               20  'XX'-N-ANZ          PIC 9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER             PIC X(091).
      *            RESERVE

