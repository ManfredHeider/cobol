      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1571.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1571.CPv  $
      *
      *   Rev 2.1   Aug 27 2003 16:25:50   t13ex27
      *
      *   Rev 2.0   May 19 2003 10:00:18   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1571 FUNKTION:Entladebestand KoKÜ-       *
      *                                   Korrektur                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: IFR                                             *
      *    DATUM:    11.08.2003                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      * FKI00766!11.08.03! 2.0! NEU FÜR KOKÜ              !I. Frenzel*
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1571-SATZ.
           05  DRKI1571-VERTRAGSID            PIC  X(013).
           05  DRKI1571-VBPERSONENID          PIC  X(013).
           05  DRKI1571-VBPERSONENROLLEKZ     PIC  X(001).
           05  DRKI1571-ABLAUFDATUM1          PIC  X(010).
           05  FILLER                         PIC  X(001).
           05  DRKI1571-GUELTIGKEITSDAT1      PIC  X(010).
           05  FILLER                         PIC  X(001).
           05  DRKI1571-AVKENNZEICHEN1        PIC  X(001).
           05  DRKI1571-LETZTEAENDERUNG1      PIC  X(026).
           05  FILLER                         PIC  X(001).
           05  DRKI1571-KOMMBETRKZ            PIC  X(001).
           05  DRKI1571-KIPERSONENID          PIC  X(013).
           05  DRKI1571-VBPERSONENROLLEKZ     PIC  X(001).
           05  DRKI1571-ABLAUFDATUM2          PIC  X(010).
           05  FILLER                         PIC  X(001).
           05  DRKI1571-GUELTIGKEITSDAT2      PIC  X(010).
           05  FILLER                         PIC  X(001).
           05  DRKI1571-AVKENNZEICHEN2        PIC  X(001).
           05  DRKI1571-LETZTEAENDERUNG2      PIC  X(026).
           05  FILLER                         PIC  X(001).
           05  DRKI1571-KIPERSONENID          PIC  X(013).
           05  DRKI1571-KIPERSONENROLLE       PIC  X(001).
           05  DRKI1571-KIKUNDENNUMMER        PIC  S9(9) COMP.
           05  FILLER                         PIC  X(001).
           05  DRKI1571-KIKUNDENSTATUSID      PIC  X(001).
           05  DRKI1571-KIANZAHLBETREUER      PIC  S9(4) COMP.
           05  FILLER                         PIC  X(001).
      ****************************************************************
