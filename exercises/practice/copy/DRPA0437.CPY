      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0437.CPY  $
      *    $Revision:   2.16  $
      *
      *    $Log:   L:/mf/CPY/DRPA0437.CPv  $
      *
      *   Rev 2.16   17 Jun 2019 10:38:58   t13ae37
      *APA02857: VSTATUS I132 bei Ges. 21 und "alter" Fälligkeit
      *
      *   Rev 2.15   May 21 2019 13:15:24   t13ae37
      *APA02857: VSTATUS I132 bei Ges. 21 und "alter" Fälligkeit
      *
      *   Rev 2.14   Apr 25 2019 12:39:20   u130076
      *APA02830: neuer PGV für Gesellschaft 33 - Capitalbank
      *
      *   Rev 2.13   01 Apr 2019 11:05:22   t13ex26
      *APA02830: neuer PGV für Gesellschaft 33 - Capitalbank
      *
      *   Rev 2.12   15 Mar 2019 11:03:22   t13ex26
      *APA02830: neuer PGV für Gesellschaft 33 - Capitalbank
      *
      *   Rev 2.11   Jun 04 2018 11:13:26   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.10   May 14 2018 11:12:08   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.9   Nov 06 2017 14:45:44   t13ex26
      *APA02492: 3. Statistik-Kategorie Folge Neu - MNZ
      *
      *   Rev 2.8   Nov 06 2017 14:03:28   t13ex26
      *APA02492: 3. Statistik-Kategorie Folge Neu - MNZ
      *
      *   Rev 2.7   Oct 23 2017 10:42:28   t13ex44
      *APA02492: 3. Statistik-Kategorie Folge Neu - MSC
      *
      *   Rev 2.6   Aug 30 2017 08:51:26   t13ae46
      *APA02499: DGV 1070: Strukturbereinigung
      *
      *   Rev 2.5   Mar 16 2011 08:59:30   t13ex25
      *APA01184: Generali Leben Österreich-LV Storno´s >=36 Monate
      *
      *   Rev 2.4   Apr 20 2005 11:05:32   t13ae46
      *APA00346: Ges. 26 - Dynamikerhöhungen
      *
      *   Rev 2.3   Sep 30 2003 15:13:48   t13ex44
      *
      *   Rev 2.2   Jul 04 2003 08:40:18   t13ex21
      *APA00238 - PE-Bereich um KI-Kundenbetreuer erweitert
      *
      *   Rev 2.1   Oct 20 1999 13:46:54   T13EX44
      *CRPA00437
      *
      *   Rev 2.0   May 27 1999 13:48:12   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:36:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0437                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<DAISY GEVO ERMITTELN>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                          *
      *    DATUM:    11.08.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.10.97! 01 ! NEU                       ! PRIGGEN  *
      * APA00012!05.02.99! 02 ! MINDERPRÄMIEN KZ EINGEFÜGT! MUND     *
      * APA00061!28.09.99! 2.1! FAELLIG-BIS-JN IN PI      ! SCHEIDEL *
      * APA00238!10.06.03! 2.2! NEU: KI-KDBETR-JN         ! STORCH   *
      * APA00346!12.04.05! 2.3! NEU: PGV-KZ               ! WANG     *
      * APA01184!28.02.11! 2.4! Neu: Neutral-KZ           ! Jäger    *
      * APA02499!22.08.17! 2.6! NEU: ANTRAG-ZUSTAND       ! WANG     *
      * APA02492!13.10.17! 2.7! AE-KZ HINZU               ! SCHEIDEL *
      * APA02492!03.11.17! 2.8! PGV+SCHICHT-NR VORB. HINZU! KRANZ    *
      * APA02579!24.04.18!2.10! NEUER RC "MFE"            ! SCHEIDEL *
      * APA02579!24.04.18!2.11! "MFE" WIEDER ENTFERNT     ! SCHEIDEL *
      * APA02830!12.03.19!2.12! PROD-KZ neu               ! KRANZ    *
      * APA02857!15.05.19!2.16! MFE wieder ergänzt        ! Buch     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0437                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0437.
          05 PI-DRPA0437-DATEN.
             10 PI-DRPA0437-PGV                     PIC S9(004) BINARY.
             10 PI-DRPA0437-FAELLIG-VON             PIC  X(010).
             10 PI-DRPA0437-DGV-VORBEWEGUNG         PIC S9(004) BINARY.
             10 PI-DRPA0437-PPFL-BEGINN             PIC  X(010).
             10 PI-DRPA0437-ANZAHL-MONATE-AP        PIC S9(009) BINARY.
             10 PI-DRPA0437-AP-FP-REGEL             PIC S9(004) BINARY.
             10 PI-DRPA0437-EE-JN                   PIC  X(001).
             10 PI-DRPA0437-AKT-PMPT-JN             PIC  X(001).
             10 PI-DRPA0437-RATIERLICH-JN           PIC  X(001).
050299       10 PI-DRPA0437-MINDERPRAEMIE-JN        PIC  X(001).
100603       10 PI-DRPA0437-KI-KDBETR-JN            PIC  X(001).
120405       10 PI-DRPA0437-PGV-KZ                  PIC  X(001).
280211       10 PI-DRPA0437-NEUTRAL-KZ              PIC  X(001).
220817       10 PI-DRPA0437-ANTRAG-ZUSTAND          PIC  9(002).
131017       10 PI-DRPA0437-AE-KZ                   PIC  X(001).
031117       10 PI-DRPA0437-PGV-VORBEWEGUNG         PIC S9(004) BINARY.
031117       10 PI-DRPA0437-SCHICHT-NR-VORBEW       PIC S9(004) BINARY.
      *
290999* DIESES KENNZEICHEN MUSS AUF "JA" GESETZT WERDEN, WENN DIE
290999* VORBEWEGUNG DASSELBE FAELLIG-BIS TRAEGT WIE DIE NEUE BEWEGUNG.
290999       10 PI-DRPA0437-FAELLIG-BIS-JN          PIC  X(001).
      *
120319       10 PI-DRPA0437-PROD-KZ                 PIC  X(001).
      *
       01 PO-DRPA0437.
          05 PO-DRPA0437-RC                         PIC  X(003).
             88 PO-DRPA0437-RC-OK                         VALUE 'OK '.
150519       88 PO-DRPA0437-RC-MFE                        VALUE 'MFE'.
          05 PO-DRPA0437-DATEN.
             10 PO-DRPA0437-DGV                     PIC S9(004) BINARY.
             10 PO-DRPA0437-PRODART                 PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 
 
 
 
 
 
 
 


