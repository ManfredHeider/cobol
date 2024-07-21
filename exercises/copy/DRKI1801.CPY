      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.05.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1801.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1801.CPv  $
      *
      *   Rev 2.4   Mar 15 2017 15:06:26   t13ex27
      *AKI01492: AML-Änderungen zum RT 03/2017-KSDS für Vertrag und
      * Riester
      *
      *   Rev 2.3   Mar 30 2009 11:57:12   t13ex48
      *AKI00767: CR 14112 - Historisierung Riesterdaten-RIESTERZULAGE
      *
      *   Rev 2.2   04 Jun 2008 16:03:48   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben Datei-Copys
      *
      *   Rev 2.1   Oct 18 2005 11:40:24   t13ex49
      *AKI00483: Riester Förderung - Sourcen und OPC
      *
      *   Rev 2.0   May 17 2005 12:41:34   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * AML - Riesterzulage KSDS
      *
      * Logbuch
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00483!17.05.05!    ! NEU                       !Peter Pohl*
      * -------------------------------------------------------------*
      * AKI00711!09.04.08!2.2 ! neu Dauerzulagen-KZ       !I. Frenzel*
      * -------------------------------------------------------------*
      * AKI00767!12.03.09!2.3 ! Erweiterung des Schluessel! HSE      *
      *         !        !    ! um Zulagen-Beitragsjahr.  !          *
      *         !        !    ! ==> Verschieben des Feldes!          *
      *         !        !    ! Kompl-AML-Zul-Beitragsjahr!          *
      * -------------------------------------------------------------*
140217* AKI01492!14.02.17!2.4 ! Neue Attribute zu RT 03/17! IFR      *
      *         !        !    ! DAUZULANTRKZN entfernt    !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRKI1801.
           05 KOMPL-AML-SA                          PIC X(01).
      *        SATZART        = 'C'
           05 KOMPL-AML-KEY.
              10 KOMPL-AML-VNR                      PIC 9(10).
      *          VERTRAGSNR
120309*   Feld nach vorne gezogen, damit es Teil des Schluessels wird
120309        10 Kompl-AML-Zul-Beitragsjahr         PIC 9(04).
      *       Beitragsjahr der Zulage JJJJ
           05 Kompl-AML-Datum                       PIC 9(08).
      *       Eingang der Zulage bzw. Zulagekorrektur JJJJMMTT
120309*    05 Kompl-AML-Zul-Beitragsjahr            PIC 9(04).
           05 Kompl-AML-Zul-Wert                    PIC S9(13)V99.
      *       Höhe der Zulage (letzte Buchung)
           05 Kompl-AML-Zul-Wert-S                  PIC S9(13)V99.
      *       Höhe der Zulage (Summe Beitragsjahr)
           05 Kompl-AML-Zul-WKZ                     PIC X(03).
      *       Währungskennzeichen
140217     05 Kompl-AML-Zul-KUERZ-SL                PIC XX.
140217*       Kürzungsschlüssel
140217     05 Kompl-AML-Zul-KEIN-ANSPR-SL           PIC XX.
140217*       Anspruchsschlüssel
140217*    05 Kompl-AML-Reserve                     PIC X(54).
140217     05 Kompl-AML-Reserve                     PIC X(51).
      *       Reservefeld
      *
      *---------------------- ENDE DRKI1801 --------------------------

