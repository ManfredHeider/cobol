      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.06.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2763.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2763.CPv  $
      *
      *   Rev 2.3   Aug 07 2013 14:05:30   t13ex73
      *AKI01204: SEPA - DB-Online IBAN Prüfprogramm
      *
      *   Rev 2.2   Aug 06 2013 08:56:12   t13ex73
      *AKI01204: SEPA - DB-Online IBAN Prüfprogramm
      *
      *   Rev 2.1   Jun 06 2013 12:04:18   t13ex73
      *AKI01204: SEPA - DB-Online - Kfz
      *
      *   Rev 2.0   Jun 05 2013 11:21:08   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01204!05.06.13! 1.0! NEU ERSTELLUNG            ! DDY      *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI01204!01.08.13!    ! SCHWERER FEHLER EINFÜGEN  ! DDY      *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRKI2763.
           05  PI-DRKI2763-DATEN.
               10 PI-DRKI2763-IBAN             PIC  X(034).
      *
       01 PO-DRKI2763.
          05 PO-DRKI2763-RC                    PIC  X(003).
      *      IBAN ist OK
             88 PO-DRKI2763-RC-OK                     VALUE 'OK '.
      *      Prüfziffer der IBAN stimmt nicht
             88 PO-DRKI2763-RC-PZ-NOK                 VALUE 'PNK'.
      *      Land der IBAN nicht gefunden
             88 PO-DRKI2763-RC-LAND-NFD               VALUE 'LNF'.
      *      Länge der IBAN passt nicht zum Land
             88 PO-DRKI2763-RC-LAENGE-NOK             VALUE 'LNK'.
      *      Die IBAN ist leer
             88 PO-DRKI2763-RC-IBAN-LEER              VALUE 'LER'.
010813*      Schwerer Fehler aus UPRO
010813       88 PO-DRKI2763-RC-IBAN-ERR               VALUE 'ERR'.
