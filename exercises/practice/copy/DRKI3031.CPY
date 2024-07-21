      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.08.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3031.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3031.CPv  $
      *
      *   Rev 2.1   Sep 22 2015 11:06:14   t13ex73
      *AKI01273: Abgleich von Kundenunterschieden zwischen OKI und 
      *HOST - Vertrag PGM + JCL
      *
      *   Rev 2.0   Aug 06 2015 08:22:20   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 3 0 3 1    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER UNLOAD DER HOSTTABELLE VERTRAG               *
      *                                                                *
      *                                                                *
      *    SATZLAENGE  114 Bytes                                       *
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
      *01  DRKI3031.
      *
           05  'XX'-DATENSATZ.
               10  'XX'-VERTRAGSID              PIC  X(026).
               10  'XX'-VERABSCHLUSSVBNR        PIC S9(009) COMP.
               10  'XX'-VERABSCHLUSSVBNR-NI     PIC  X(001).
                   88 'XX'-VERABSCHLUSSVBNR-IN          VALUE '?'.
               10  'XX'-MITARBEITERNR           PIC S9(009) COMP.
               10  'XX'-MITARBEITERNR-NI        PIC  X(001).
                   88 'XX'-MITARBEITERNR-IN             VALUE '?'.
               10  'XX'-VEREHELEUTEKZ           PIC  X(001).
               10  'XX'-VEREHELEUTEKZ-NI        PIC  X(001).
                   88 'XX'-VEREHELEUTEKZ-IN             VALUE '?'.
               10  'XX'-VERFIRMENVTGKZ          PIC  X(001).
               10  'XX'-VERFIRMENVTGKZ-NI       PIC  X(001).
                   88 'XX'-VERFIRMENVTGKZ-IN            VALUE '?'.
               10  'XX'-VERGEBUENDELTKZ         PIC  X(001).
               10  'XX'-VERGESAMTBEITRAG        PIC S9(009)V9(2) COMP-3.
               10  'XX'-VERGESAMTBEITRAG-NI     PIC  X(001).
                   88 'XX'-VERGESAMTBEITRAG-IN          VALUE '?'.
               10  'XX'-VERGESBEITRWKZ          PIC  X(001).
               10  'XX'-VERMANDANTNR            PIC S9(009) COMP.
               10  'XX'-VERPARTGESELLSCHID      PIC S9(004) COMP.
               10  'XX'-VERVERTRAGSABLAUF       PIC  X(010).
               10  'XX'-VERVERTRAGSABLAUF-NI    PIC  X(001).
                   88 'XX'-VERVERTRAGSABLAUF-IN         VALUE '?'.
               10  'XX'-VERVERTRAGSBEGINN       PIC  X(010).
               10  'XX'-VERVERTRAGSBEGINN-NI    PIC  X(001).
                   88 'XX'-VERVERTRAGSBEGINN-IN         VALUE '?'.
               10  'XX'-VERVERTRZUSTANDID       PIC S9(004) COMP.
               10  'XX'-VERVERTRZUSTANDID-NI    PIC  X(001).
                   88 'XX'-VERVERTRZUSTANDID-IN         VALUE '?'.
               10  'XX'-VERZAHLUNGSARTID        PIC S9(004) COMP.
               10  'XX'-VERZAHLUNGSARTID-NI     PIC  X(001).
                   88 'XX'-VERZAHLUNGSARTID-IN          VALUE '?'.
               10  'XX'-VRBKOMMBETRKZ           PIC  X(001).
               10  'XX'-VERZAHLUNGSWEISEID      PIC S9(004) COMP.
               10  'XX'-VERZAHLUNGSWEISEID-NI   PIC  X(001).
                   88 'XX'-VERZAHLUNGSWEISEID-IN        VALUE '?'.
               10  'XX'-LETZTE-AEND-TS          PIC  X(026).
               10  'XX'-FILLER-1                PIC  X(001).
      *

