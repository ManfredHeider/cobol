      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.08.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3032.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3032.CPv  $
      *
      *   Rev 2.1   Sep 22 2015 11:06:18   t13ex73
      *AKI01273: Abgleich von Kundenunterschieden zwischen OKI und 
      *HOST - Vertrag PGM + JCL
      *
      *   Rev 2.0   Aug 06 2015 08:22:32   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 3 0 3 2    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER UNLOAD DER OKITABELLE VERTRAG                *
      *                                                                *
      *                                                                *
      *    SATZLAENGE  145 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *05.08.15!NEUERSTELLUNG                          !DDY      ! 01  *
      *AKI01273!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
      *01  DRKI3032.
      *
           05  'XX'-DATENSATZ.
               10  'XX'-VERTRAGSID              PIC  X(026).
               10  'XX'-VERABSCHLUSSVBNR        PIC  9(009).
               10  'XX'-VERABSCHLUSSVBNR-NI     PIC  X(001).
                   88 'XX'-VERABSCHLUSSVBNR-IN          VALUE '?'.
               10  'XX'-MITARBEITERNR           PIC  9(009).
               10  'XX'-MITARBEITERNR-NI        PIC  X(001).
                   88 'XX'-MITARBEITERNR-IN             VALUE '?'.
               10  'XX'-VEREHELEUTEKZ           PIC  X(001).
               10  'XX'-VEREHELEUTEKZ-NI        PIC  X(001).
                   88 'XX'-VEREHELEUTEKZ-IN             VALUE '?'.
               10  'XX'-VERFIRMENVTGKZ          PIC  X(001).
               10  'XX'-VERFIRMENVTGKZ-NI       PIC  X(001).
                   88 'XX'-VERFIRMENVTGKZ-IN            VALUE '?'.
               10  'XX'-VERGEBUENDELTKZ         PIC  X(001).
               10  'XX'-VERGESAMTBEITRAG        PIC +9(009)V9(2).
               10  'XX'-VERGESAMTBEITRAG-NI     PIC  X(001).
                   88 'XX'-VERGESAMTBEITRAG-IN          VALUE '?'.
               10  'XX'-VERGESBEITRWKZ          PIC  X(001).
               10  'XX'-VERGESBEITRWKZ-NI       PIC  X(001).
                   88 'XX'-VERGESBEITRWKZ-IN            VALUE '?'.
               10  'XX'-VERMANDANTNR            PIC  9(009).
               10  'XX'-VERPARTGESELLSCHID      PIC  9(004).
               10  'XX'-VERVERTRAGSABLAUF       PIC  X(010).
               10  'XX'-VERVERTRAGSABLAUF-NI    PIC  X(001).
                   88 'XX'-VERVERTRAGSABLAUF-IN         VALUE '?'.
               10  'XX'-VERVERTRAGSBEGINN       PIC  X(010).
               10  'XX'-VERVERTRAGSBEGINN-NI    PIC  X(001).
                   88 'XX'-VERVERTRAGSBEGINN-IN         VALUE '?'.
               10  'XX'-VERVERTRZUSTANDID       PIC +9(004).
               10  'XX'-VERZAHLUNGSARTID        PIC +9(004).
               10  'XX'-VRBKOMMBETRKZ           PIC  X(001).
               10  'XX'-VRBKOMMBETRKZ-NI        PIC  X(001).
                   88 'XX'-VRBKOMMBETRKZ-IN             VALUE '?'.
               10  'XX'-VERZAHLUNGSWEISEID      PIC +9(004).
               10  'XX'-LETZTE-AEND-TS          PIC  X(026).
               10  FILLER                       PIC  X(001).
      *

