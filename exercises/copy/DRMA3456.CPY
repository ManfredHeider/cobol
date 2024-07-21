      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3456.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA3456.CPv  $
      *
      *   Rev 2.2   May 18 2016 11:46:30   t13ac17
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.1   Jul 01 2013 17:13:56   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz für Druckstücke zur Erfolgsprovisi
      *
      *     SOURCE-NAME      :  DRMA3456
      *
      *     GENERIERUNG      :  03.05.2016/15:42:23
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00270!06.04.16!2.2  ! NEU: Umsatzsteuerfelder   !   Walbert
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRUCK-ERFPROV-SATZ.
              07 VBNAME                 pic X(20).
              07 STRUK-STUFE-TEXT       pic X(4).
              07 VBNR                   pic X(7).
              07 DIR-NR                 pic S9(4)      binary.
              07 PRAXIS-STUFE           pic S9(4)      binary.
              07 AE-MONATS-DS-GRENZE    pic S9(7)V99   packed-decimal.
              07 AE-MONATS-DS           pic S9(7)V99   packed-decimal.
              07 AE-MONATS-DS-UST-FREI  pic S9(7)V99   packed-decimal.
              07 AE-MONATS-DS-UST-PFL   pic S9(7)V99   packed-decimal.
              07 MONAT-1                pic X(7).
              07 MONAT-2                pic X(7).
              07 MONAT-3                pic X(7).
              07 AE-MONAT-1-UST-FREI    pic S9(7)V99   packed-decimal.
              07 AE-MONAT-1-UST-PFL     pic S9(7)V99   packed-decimal.
              07 AE-MONAT-2-UST-FREI    pic S9(7)V99   packed-decimal.
              07 AE-MONAT-2-UST-PFL     pic S9(7)V99   packed-decimal.
              07 AE-MONAT-3-UST-FREI    pic S9(7)V99   packed-decimal.
              07 AE-MONAT-3-UST-PFL     pic S9(7)V99   packed-decimal.
              07 FAKTOR-EP              pic S9(7)V99   packed-decimal.
              07 BRUTTO-BETRAG-EP-UST-FREI
                                        pic S9(7)V99   packed-decimal.
              07 BRUTTO-BETRAG-EP-UST-PFL
                                        pic S9(7)V99   packed-decimal.
              07 EQF                    pic S999V99    packed-decimal.
              07 STORMIND-PROZ          pic S999V99    packed-decimal.
              07 KUERZUNG-UST-FREI      pic S9(7)V99   packed-decimal.
              07 KUERZUNG-UST-PFL       pic S9(7)V99   packed-decimal.
              07 AUSZAHLUNG-EP-UST-FREI pic S9(7)V99   packed-decimal.
              07 AUSZAHLUNG-EP-UST-PFL  pic S9(7)V99   packed-decimal.
              07 KNR-VERGUET-TYP        pic S9(4)      binary.
              07 WERTUNGSZEITRAUM       pic 9.
              07 AE-MONATS-DS-EP-UST-FREI
                                        pic S9(7)V99   packed-decimal.
              07 AE-MONATS-DS-EP-UST-PFL
                                        pic S9(7)V99   packed-decimal.
              07 EQF-KUERZUNG-VON       pic S99V9      packed-decimal.
              07 EQF-KUERZUNG-BIS       pic S99V9      packed-decimal.
      *
      **************************************************************

