      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.05.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1100.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB1100.CPv  $
      *
      *   Rev 2.3   Dec 08 2016 15:14:04   t13ex71
      *AVB01400: Projekt Lucy2 - Neue Funktion 3 in UPVB0189 für Fi
      *bu Jahresultimo aufnehmen (BPR)
      *
      *   Rev 2.2   Jun 30 2016 11:51:14   t13ex71
      *AVB01400: Projekt Lucy2 - Neue Funktion in UPVB0189 aufnehme
      *n (BPR)
      *
      *   Rev 2.1   Jun 02 2015 15:01:12   t13ex71
      *AVB01347: Unterprogramm UPVB0189 zum Ermitteln steuerlicher
      *Status für Daisy
      *
      *   Rev 2.0   May 21 2015 11:20:44   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1100                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPVB0189                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRETSCH                                          *
      *    DATUM:    21.05.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01347!21.05.15! 01 ! NEU                       !B.PRETSCH *
      * AVB01400!24.05.16! 02 ! ADD Funktion DRVB1141     !B.PRETSCH *
      * AVB01400!30.11.16! 03 ! ADD Funktion DRVB1157     !B.PRETSCH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-UPVB0189.
           05  PI-UPVB0189-FUNKTION            PIC  X(008).
      *--- Funktion 1: Ermittlung steuerl. Status für Daisy
               88 PI-UPVB0189-DRVB1099              VALUE 'DRVB1099'.
      *--- Funktion 2: Ermittlung steuerl. Status für Fibu
               88 PI-UPVB0189-DRVB1141              VALUE 'DRVB1141'.
      *--- Funktion 3: Ermittlung steuerl. Status für Fibu zur
      *---             Jahresultimo-Abrechnung
               88 PI-UPVB0189-DRVB1157              VALUE 'DRVB1157'.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

