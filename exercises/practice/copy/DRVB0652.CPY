      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0652.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRVB0652.CPv  $
      *
      *   Rev 2.6   Sep 10 2020 13:12:52   i13pcch
      *AAS01291: Auftrag für Azubis bei VB Leistung (VB - System): 
      *Konstante K-VERS-I1 ersetzt durch zweistellige Jahresangabe
      *
      *   Rev 2.5   Feb 22 2006 07:42:12   t13ex43
      *AVB00840: PGM - Jährliche §109a-Aktion 2005
      *
      *   Rev 2.4   Jul 20 2004 12:48:32   t13ex43
      *AVB00745: Nachtrag Konstantenänderung
      *
      *   Rev 2.3   Jun 03 2004 12:06:58   t13ex43
      *PGM 2 - Meldung §109A geänderter Satzaufbau
      *
      *   Rev 2.2   May 28 2004 17:23:10   t13ex43
      *PGM - Meldung §109A geänderter Satzaufbau
      *
      *   Rev 2.1   Apr 22 2003 16:23:14   t13ae35
      *AVB00618 - neu, Konstanten für Info und Meldungssätze
      *
      *   Rev 2.0   Feb 17 2003 10:57:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *   COPY-MEMBER: fachliche Konstanten für Info-und Meldungs-   *
      *                sätze für die österr. Finanzbehörde           *
      *--------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB00618!04.04.03!2.1 ! NEU                       ! J.Hering *
130504* AVB00745!13.05.04!2.2 ! NEUE KONSTANTEN FUER ELDA ! KLEEMEIER*
030604* AVB00745!03.06.04!2.3 ! ELDA-INFOSATZ             ! KLEEMEIER*
200704* AVB00745!20.07.04!2.4 ! NACHTRAG KONSTANTENAENDER.! KLEEMEIER*
080206* AVB00840!08.02.06!2.5 ! ÄNDERUNGEN FÜR 2005       ! KLEEMEIER*
280720* VLA479  !28.07.20!2.6 ! K-VERS-I1 auskommentiert  ! D.ZEISLER*
      *                                                              *
      ****************************************************************
      *
       01  FILLER.
      *-------------------------------------------------------------*
      *      Konstanten für Infosatz                                *
      *-------------------------------------------------------------*
           05  K-INFO-SA              PIC  X(001)       VALUE     'I'.
           05  K-INFO-FINAMT-NR2      PIC  9(002)       VALUE      09.
           05  K-INFO-STEUER-NR2      PIC  9(007)       VALUE 0181058.
           05  K-INFO-DVR-NR3         PIC  9(007)       VALUE 0009091.
           05  K-INFO-ART             PIC  X(002)       VALUE    'LZ'.
           05  K-INFO-STRUK-VERSION   PIC  X(002)       VALUE    '03'.
           05  K-INFO-AUFGEB-NAME     PIC  X(037)
                                VALUE 'Deutsche Vermögensberatung AG'.
           05  K-INFO-AUFGEB-ADRESSE  PIC  X(037)
                                        VALUE 'Rotenturmstraße 16-18'.
           05  K-INFO-AUFGEB-LAND     PIC  X(003)       VALUE   'A  '.
           05  K-INFO-AUFGEB-PLZ      PIC  X(010)
                                                   VALUE '1010      '.
130504     05  K-INFO-AUFGEB-ORT      PIC  X(020)  VALUE 'Wien      '.
      *-------------------------------------------------------------*
      *      Konstanten für Meldungssätze                           *
      *-------------------------------------------------------------*
           05  K-MELD-SA              PIC  X(001)       VALUE     'W'.
           05  K-MELD-LSTG-ART        PIC  X(002)       VALUE    '02'.
           05  K-MELD-FINAMT-NR3      PIC  9(002)       VALUE      09.
           05  K-MELD-STEUER-NR3      PIC  9(007)       VALUE 0181058.
           05  K-MELD-DVR-NR3-NUM     PIC  9(007)       VALUE 0747017.
           05  K-MELD-VB-TEL-NR-NUM-T2  PIC  9(010)    VALUE 0017205055.
           05  K-MELD-VB-TELEFAX-NUM-T2 PIC  9(010)    VALUE 0017205048.
      *-------------------------------------------------------------*
      *      ELDA-Konstanten                                        *
      *-------------------------------------------------------------*
130504     05  K-SART-00            PIC X(002)       VALUE '00'.
030604     05  K-SART-I1            PIC X(002)       VALUE 'I1'.
130504     05  K-SART-W1            PIC X(002)       VALUE 'W1'.
130504     05  K-SART-99            PIC X(002)       VALUE '99'.
130504     05  K-UVST               PIC X(002)       VALUE '14'.
200704***  05  K-VSTR               PIC X(002)       VALUE '14'.
200704     05  K-VSTR               PIC X(002)       VALUE '94'.
130504     05  K-PROJ               PIC X(002)       VALUE 'TM'.
030604     05  K-PROJ-DM            PIC X(002)       VALUE 'DM'.
200704***  05  K-BEST               PIC X(002)       VALUE 'BG'.
200704     05  K-BEST               PIC X(002)       VALUE 'LF'.
080206***  05  K-VERS               PIC 9(002)       VALUE 02.
280720**   05  K-VERS-I1            PIC 9(002)       VALUE 04.
130504     05  K-FSART              PIC X(001)       VALUE 'W'.
030604     05  K-FSART-I            PIC X(001)       VALUE 'I'.
130504     05  K-ARTE               PIC X(002)       VALUE '02'.
 

