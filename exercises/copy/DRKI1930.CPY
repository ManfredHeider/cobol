      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.07.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1930.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1930.CPv  $
      *
      *   Rev 2.1   Oct 05 2006 15:49:12   t13ex14
      *AKI00612: Migration Kommunikation
      *
      *   Rev 2.0   Jul 04 2006 11:37:26   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00546!03.02.06!2.1 ! neu                       !I. Frenzel*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1930-SATZ.
           05  DRKI1930-HAUSHALTID                PIC  X(013).
           05  DRKI1930-NATPHBPERSONENID          PIC  X(013).
           05  DRKI1930-NATPHBREIHENFOLGE         PIC S9(004) COMP.
           05  DRKI1930-KOMMTELARTID              PIC S9(004) COMP.
           05  DRKI1930-KOMMLAUFENDENUMMER        PIC S9(009) COMP.
           05  DRKI1930-KOMMNUMMER.
               49 DRKI1930-KOMMNUMMER-LEN         PIC S9(004) COMP.
               49 DRKI1930-KOMMNUMMER-TEXT        PIC  X(128).
      ****************************************************************
