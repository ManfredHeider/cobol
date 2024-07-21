      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.05.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1796.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1796.CPv  $
      *
      *   Rev 2.3   Jul 16 2015 15:07:12   t13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Source
      *n und Copies ASR
      *
      *   Rev 2.2   May 21 2014 13:53:42   t13ae37
      *APA01766: Kontrollsummen DAISY: zus. Funktion in ZWPA0034
      *
      *   Rev 2.1   May 21 2014 12:42:30   t13ae37
      *APA01766: Kontrollsummen DAISY: zus. Funktion in ZWPA0034
      *
      *   Rev 2.0   May 21 2014 09:36:36   t13ae37
      *Archiverstellung
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01766!21.05.14! 2.1! NEUERSTELLUNG             ! BUCH     *
      * APA01932!24.04.15! 2.2! LUCY: UST FÜR GES. 90 DV  ! STRAUSS  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1796                                 *
      ****************************************************************
       01 PI-DRPA1796.
          05 PI-DRPA1796-PROGRAMM               PIC  X(008).
          05 PI-DRPA1796-UFKT                   PIC  9(002).
          05 PI-DRPA1796-KNR-VGES               PIC S9(004) BINARY.
      *
       01 PO-DRPA1796.
          05 PO-DRPA1796-RC                     PIC  X(003).
             88 PO-DRPA1796-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1796-RC-EOF                   VALUE 'EOF'.
             88 PO-DRPA1796-RC-LEER                  VALUE 'LER'.
          05 PO-DRPA1796-DATEN.
             10 PO-DRPA1796-KNR-VGES            PIC S9(004) BINARY.
             10 PO-DRPA1796-PGEB-GES            PIC S9(004) BINARY.
             10 PO-DRPA1796-KOSU-PROVISION      PIC S9(009)V9(2) COMP-3.
             10 PO-DRPA1796-KOSU-AUSZAHLUNG     PIC S9(009)V9(2) COMP-3.
             10 PO-DRPA1796-KOSU-RUECKSTELLUNG  PIC S9(009)V9(2) COMP-3.
240415       10 PO-DRPA1796-KOSU-UST-GESAMT     PIC S9(009)V9(2) COMP-3.
240415       10 PO-DRPA1796-KOSU-UST-AUSZAHL    PIC S9(009)V9(2) COMP-3.
      *
      *---------------------------------------------------------------*

