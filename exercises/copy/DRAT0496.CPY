      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.11.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0496.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAT0496.CPv  $
      *
      *   Rev 2.5   Jun 21 2013 12:16:50   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN - Einarbeitu
      *ng IBAN-Regelwerk in IBAN-Konverter 1.0 UPRW0001
      *
      *   Rev 2.4   Jun 21 2013 11:04:54   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN - Einarbeitu
      *ng IBAN-Regelwerk in IBAN-Konverter 1.0 UPRW0001
      *
      *   Rev 2.3   Jun 19 2013 11:42:28   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN - Einarbeitu
      *ng IBAN-Regelwerk in IBAN-Konverter 1.0 UPRW0001
      *
      *   Rev 2.2   Feb 21 2013 15:10:02   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.1   Nov 19 2012 16:43:14   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.0   Nov 15 2012 19:02:54   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0496                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWAT0035                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01244!15.11.12! 01 ! NEUERSTELLUNG             !B.PRETSCH *
      * AVB01244!21.05.13! 02 ! IBAN-REGELN AUFNEHMEN     !B.PRETSCH *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0496                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0496.
          05 PI-DRAT0496-LZ                 PIC S9(004) COMP.
          05 PI-DRAT0496-BLZ                PIC  X(011) .
      *
       01 PO-DRAT0496.
          05 PO-DRAT0496-RC                 PIC  X(003) .
             88 PO-DRAT0496-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0496-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRAT0496-DATEN.
            10 PO-DRAT0496-BIC              PIC  X(011) .
            10 PO-DRAT0496-AEND-KZ          PIC  X(001) .
            10 PO-DRAT0496-LOESCH-KZ        PIC S9(004) COMP.
            10 PO-DRAT0496-IBAN-REGEL       PIC S9(009) COMP.
            10 PO-DRAT0496-FOLGE-BLZ        PIC  X(011) .
            10 PO-DRAT0496-SATZNUMMER       PIC S9(009) COMP.
      *
      ***** Ende Copy-Member DRAT0496 *****************************
