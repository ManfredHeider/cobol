      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  18.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5171.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5171.CPv  $
      *
      *   Rev 2.1   Oct 15 2018 17:58:46   t13ac19
      *AMA00412: Neue Kriterien Qualitätsmanagement bzw. Umsatzsteu
      *er des VB 
      *
      *   Rev 2.0   Sep 18 2018 19:21:22   t13ac19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     MITCOPY: SUMMENBLATT DIVERSE JAHRESFELDER
      *
      *     SOURCE-NAME      :  DRMA5171
      *
      *     GENERIERUNG      :  20.09.2018/15:57:21
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00412!18.09.18!2.1  ! NEUERSTELLUNG             ! O.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5171.
              07 VBNR                   pic X(7).
              07 BUCH-DAT               pic X(10).
              07 LEBEN-1                pic S9(9)V99   packed-decimal.
              07 SACH-1                 pic S9(9)V99   packed-decimal.
              07 DYNAMIK-1              pic S9(9)V99   packed-decimal.
              07 SONST-VERS-BS-1        pic S9(9)V99   packed-decimal.
              07 INVESTMENT-1           pic S9(9)V99   packed-decimal.
              07 KREDIT-1               pic S9(9)V99   packed-decimal.
              07 SONSTIGE-BANK-1        pic S9(9)V99   packed-decimal.
              07 PARTNER-DVAG-WIEN-1    pic S9(9)V99   packed-decimal.
              07 ERFOLGSPROVISION-1     pic S9(9)V99   packed-decimal.
              07 FREIW-SONDERPROV-1     pic S9(9)V99   packed-decimal.
              07 SONST-PROVISIONEN-1    pic S9(9)V99   packed-decimal.
              07 ED-PROVISION-1         pic S9(9)V99   packed-decimal.
              07 FREI-1                 pic S9(9)V99   packed-decimal.
              07 ZWISCHENSUMME-USTFREI-1
                                        pic S9(9)V99   packed-decimal.
              07 SONST-PRODUKTE-PFL-2   pic S9(9)V99   packed-decimal.
              07 SONST-PRODUKTE-NE-2    pic S9(9)V99   packed-decimal.
              07 ERFOLGSPROVISION-PFL-2 pic S9(9)V99   packed-decimal.
              07 ERFOLGSPROVISION-NE-2  pic S9(9)V99   packed-decimal.
              07 FREIW-SONDERPROV-PFL-2 pic S9(9)V99   packed-decimal.
              07 FREIW-SONDERPROV-NE-2  pic S9(9)V99   packed-decimal.
              07 SONST-PROVISIONEN-PFL-2
                                        pic S9(9)V99   packed-decimal.
              07 SONST-PROVISIONEN-NE-2 pic S9(9)V99   packed-decimal.
              07 ED-PROVISION-PFL-2     pic S9(9)V99   packed-decimal.
              07 ED-PROVISION-NE-2      pic S9(9)V99   packed-decimal.
              07 FREI-PFL-2             pic S9(9)V99   packed-decimal.
              07 FREI-NE-2              pic S9(9)V99   packed-decimal.
              07 ZWISCHENSUMME-USTPFL-2 pic S9(9)V99   packed-decimal.
      *
      **************************************************************

