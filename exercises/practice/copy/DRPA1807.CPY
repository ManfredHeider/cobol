      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1807.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1807.CPv  $
      *
      *   Rev 2.2   Oct 12 2016 09:47:48   t13ae37
      *APA02326: neuer ABR-NR Zugriff in Korrekturprogramm
      *
      *   Rev 2.1   Jul 16 2015 15:06:10   t13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Source
      *n und Copies ASR
      *
      *   Rev 2.0   Apr 01 2015 15:43:28   t13ae39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *   PARAMETERBESCHREIBUNG                                       *
      *   IN     : PI-ZTPA0138                                        *
      *            PI-EINGABE                                         *
      *            STANDARD-STATUS-BEREICH                            *
      *   OUT    : PO-RUECKGABE                                       *
      *   IN/OUT : ERR-ERROR-BEREICH                                  *
      *                                                               *
      *   VERWENDETE UEBERGABEBEREICHE:                               *
      *                                                               *
      *   COPY : DRPA1807 - Z. B. FUNKTIONSNAME                       *
      *   IN   : PI-ZTPA0138                                          *
      *                                                               *
      *   COPY : DRPA1808 - UEBERGABEBEREICH FUER INSERT              *
      *   IN   : PI-DRPA1808                                          *
      *   OUT  : PO-DRPA1808                                          *
      *                                                               *
      *   COPY : DRPA1809 - UEBERGABEBEREICH FUER UPDATE              *
      *   IN   : PI-DRPA1809                                          *
      *   OUT  : PO-DRPA1809                                          *
      *                                                               *
      *   COPY : DRPA1810 - UEBERGABEBEREICH FUER SELECT ALL          *
      *   IN   : PI-DRPA1810                                          *
      *   OUT  : PO-DRPA1810                                          *
      *                                                               *
      *   COPY : DRPA1811 - UEBERGABEBEREICH FUER DELETE              *
      *   IN   : PI-DRPA1811                                          *
      *   OUT  : PO-DRPA1811                                          *
      *                                                               *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA01932!01.04.15! 2.0! NEUERSTELLUNG             ! STRAUSS   *
      * APA02326!04.10.16! 2.1! DRPA1812 ERGÄNZT          ! BUCH      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
       01  PI-ZTPA0138.
           05 PI-ZTPA0138-FUNKTION            PIC X(008).
              88 PI-ZTPA0138-DRPA1808-INS         VALUE 'DRPA1808'.
              88 PI-ZTPA0138-DRPA1809-UPD         VALUE 'DRPA1809'.
              88 PI-ZTPA0138-DRPA1810-SEL-ALL     VALUE 'DRPA1810'.
              88 PI-ZTPA0138-DRPA1811-DEL         VALUE 'DRPA1811'.
041016        88 PI-ZTPA0138-DRPA1812-SEL-KORR    VALUE 'DRPA1812'.
      *****************************************************************
 

