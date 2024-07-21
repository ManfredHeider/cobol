      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.05.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1795.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1795.CPv  $
      *
      *   Rev 2.2   Jul 16 2015 15:07:04   t13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Source
      *n und Copies ASR
      *
      *   Rev 2.1   May 20 2014 11:37:58   t13ae37
      *APA01766: Kontrollsummen DAISY: zus. Funktion in ZWPA0034
      *
      *   Rev 2.0   May 20 2014 10:00:54   t13ae37
      *Archiverstellung
      *
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01766!20.05.14! 2.2! NEUERSTELLUNG             ! BUCH     *
      * APA01932!24.04.15! 2.3! LUCY: UST FÜR GES. 90 DV  ! STRAUSS  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1795                                 *
      ****************************************************************
       01 PI-DRPA1795.
          05 PI-DRPA1795-PROGRAMM               PIC  X(008).
          05 PI-DRPA1795-UFKT                   PIC  9(002).
          05 PI-DRPA1795-KNR-VGES               PIC S9(004) BINARY.
      *
       01 PO-DRPA1795.
          05 PO-DRPA1795-RC                     PIC  X(003).
             88 PO-DRPA1795-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1795-RC-EOF                   VALUE 'EOF'.
             88 PO-DRPA1795-RC-LEER                  VALUE 'LER'.
          05 PO-DRPA1795-DATEN.
             10 PO-DRPA1795-KNR-VGES            PIC S9(004) BINARY.
             10 PO-DRPA1795-KOSU-PROVISION      PIC S9(009)V9(2) COMP-3.
             10 PO-DRPA1795-KOSU-AUSZAHLUNG     PIC S9(009)V9(2) COMP-3.
             10 PO-DRPA1795-KOSU-RUECKSTELLUNG  PIC S9(009)V9(2) COMP-3.
240415       10 PO-DRPA1795-KOSU-UST-GESAMT     PIC S9(009)V9(2) COMP-3.
240415       10 PO-DRPA1795-KOSU-UST-AUSZAHL    PIC S9(009)V9(2) COMP-3.
      *
      *---------------------------------------------------------------*

