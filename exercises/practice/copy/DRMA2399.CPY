      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2399.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA2399.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:11:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------+-----------------------*
      * Autor    : Walbert, Gaby              ! Erstellt : 17.04.2013 *
      *            SP-Atlas                   !                       *
      *---------------------------------------+-----------------------*
      * Modultyp     : COBCOPY                                        *
      *---------------------------------------------------------------*
      * Bezeichnung  : ZPA - DTA  (Satzatz C)                         *
      *---------------------------------------------------------------*
      *
      *
           05 SATZART              pic X        value 'C'.
      * Wert BIC Deutsche Bank, Schoeller Bank
           05 BIC-L                pic X(11)    value space.
           05 BIC-G                pic X(11)    value space.
           05 IBAN-G               pic X(34)    value space.
           05 KUNDENNUMMER-INTERN  pic X(06)    value low-value.
           05                      pic S9(13)   packed-decimal value 0.
           05 TEXTSCHLUESSEL       pic 9(05)    packed-decimal
                                                value 51000.
           05                      pic X        value space.
      * Wert BIC Deutsche Bank, Schoeller Bank
           05 BIC-DVAG             pic X(11)    value space.
           05 IBAN-C               pic X(34)    value space.
           05 UEBERW-G-EURO        pic 9(09)V99 packed-decimal value 0.
           05                      pic XXX      value space.
           05 NAME-G               pic X(27)    value space.
           05 NAME-L               pic X(27)    value space.
           05 VERWENDUNGSZWECK.
              10 VBNR-1-7-PRZ      pic X(08)    value space.
              10                   pic X(09)    value ' ABR. V. '.
              10 ABRDATUM-X        pic X(10)    value space.
           05 WAEHRUNGSKZN         pic X        value space.
           05                      pic XX       value space.
           05 ERWEITERUNGSKZN      pic 99       packed-decimal value 0.
