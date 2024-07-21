      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2073.CPY  $                                 00010000



      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2073.CPv  $
      *
      *   Rev 2.5   Jul 06 2020 13:47:28   u130176
      *AKI01789: Anpassungen Kundenbestandsvolumen für AS-Club (Ums
      *etzung)
      *
      *   Rev 2.4   Mar 12 2015 13:32:22   t13ex15
      *AKI01337: Änderung KLU Sonderleistungen 2015 Anpassung VBI S
      *ST und AS-Club
      *
      *   Rev 2.3   Jul 14 2014 12:08:10   t13ex15
      *AKI01303: "VEP"-Wertung für AS-Club und KLZ Programme und Co
      *pys
      *
      *   Rev 2.2   Feb 01 2011 10:06:32   t13ex15
      *AKI01005: Änderung AS-Club-Bedingungen (Investment) PMKI0422
      *
      *   Rev 2.1   Oct 12 2007 09:52:54   t13ex50
      *AKI00630: Einmalbeitragsversicherungen in der AS-Club-Wertung
      *
      *   Rev 2.0   Sep 13 2007 14:55:52   t13ex50
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI2073                                     *
      *                                                              *
      *    FUNKTION: COPY FUER AUSGABESATZ SIEHE UNTEN (nur PMKI1180)*
      *    -------------------------------------------------------   *
      *    ERSTELLT: S. Warz                                         *
      *    DATUM:    13.09.2007                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00630!13.09.07! 01 !NEU                        !SWZ       *
      * -------------------------------------------------------------*
      * AKI01005!31.01.11! 2.1!Anpassung Investment       !SWZ       *
      * -------------------------------------------------------------*
      * AKI01303!14.07.14! 2.2!Anpassung VEP              !SWZ       *
      * -------------------------------------------------------------*
180215* AKI01337!18.02.15! 2.3!Anpassung KLZ              !SWZ       *
      * -------------------------------------------------------------*
      * AKI01789!18.06.20! 2.4!Ausslussgrund hinzu        !BLO       *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      * COBOLCOPY FÜR DIE AUSGABESATZART DER AS-CLUB-VERARBEITUNG
      *   (EINMALLAUF) -> ERWEITERTE STANDARDCOPY
      *     SATZLAENGE: 100 Stellen
      *     FORMAT:     F
      *---------------------------------------------------------------*
      *    FELDER DER SATZART
      *         - VBNR / 7-STELLIGE BERATERNUMMER
      *         - VNID / KÜNSTLICHER SCHLÜSSEL PERSONEN-ID
      *         - NUMMER DER PARTNERGESELLSCHAFT
      *         - (EXTERNE) VERTRAGSNUMMER
      *         - FOLGENUMMER
      *         - PRAEMIENBETRAG FÜR DEN OBIGEN (DIESEN) SCHLÜSSEL
      *         - KUNDENNUMMER
      *         - VERTRAGS-ID / KÜNSTLICHER SCHLÜSSEL
      *         - KDAS-TYP
      *         - AUSSCHLUSSGRUND
      *---------------------------------------------------------------*
       01  'XX'-FILE-SATZ.
           05 'XX'-VBNR                        PIC 9(7) COMP-3.
           05 'XX'-VNID                        PIC X(13).
           05 'XX'-GES                         PIC S9(04) COMP.
           05 'XX'-VERTRAGSNR                  PIC X(15).
           05 'XX'-FOLGENR                     PIC 99.
           05 'XX'-PRAEMIE                     PIC S9(9)V99 COMP-3.
191110     05 'XX'-PRAEMIE-GHA                 PIC S9(9)V99 COMP-3.
191110     05 'XX'-PRAEMIE-VKOTO-TOPZINS       PIC S9(9)V99 COMP-3.
140714     05 'XX'-PRAEMIE-AML-VEP             PIC S9(9)V99 COMP-3.
           05 'XX'-VNNR                        PIC S9(9) COMP.
           05 'XX'-VERTRAGSID                  PIC X(13).
180215     05 'XX'-TYP                         PIC S9(4) COMP-5.
           05 'XX'-AUSSCHLGRUND                PIC S9(2).
180215     05 FILLER                           PIC X(19).
 


