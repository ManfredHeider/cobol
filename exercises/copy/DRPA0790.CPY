      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0790.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA0790.CPv  $
      *
      *   Rev 2.7   Mar 28 2017 08:19:44   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs 
      *
      *   Rev 2.6   Jul 16 2015 15:08:26   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.5   Mar 10 2014 16:47:00   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.4   Jul 26 2013 11:42:00   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.3   Jun 19 2012 09:25:44   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:50:38   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 10:52:24   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Feb 27 1999 14:29:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:19:24   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   12 Dec 1997 09:47:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0790                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  VERTRAG UPDATEN                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    11.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.01.98! 01 ! NEU                       ! STORCH   *
      * APP00012!16.07.98! 1.1! PVER-KENNUNG HINZU        ! PARENT   *
      * APA00012!31.07.98!    ! NEU: AVERM & VM           ! PARENT   *
      *         !        !    ! V-GUELT-BIS GUELT-AB      !          *
      *         !        !    ! V-KENNUNG, VORVERTRAG     !          *
      * APA00622!23.05.07! 2.1! NEU: KNR-VGES             ! STORCH   *
      * APA01101!23.02.12! 2.2! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    ! *-BEREICHE ENTFERNT       !          *
260713* APA01539!26.07.13! 2.4! ERGÄNZUNGEN FÜR KALYPSO   ! RAHNER   *
      * APA01932!13.05.15! 2.6! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      * APA02353!20.02.17! 2.7! EINFÜHRUNG Teleson        ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0790                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0790.
          05 PI-DRPA0790-PROGRAMM              PIC  X(008).
          05 PI-DRPA0790-UFKT                  PIC  9(002).
          05 PI-DRPA0790-PVER-VERSION-ALT      PIC S9(4) USAGE COMP.
          05 PI-DRPA0790-PVER-DATEN.
             10 PI-DRPA0790-PRINTE             PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0790-PVER-VERSION       PIC S9(4) USAGE COMP.
             10 PI-DRPA0790-PVER-F-GUELTIG-AB  PIC X(10).
             10 PI-DRPA0790-PVER-F-GUELTIG-BIS PIC X(10).
             10 PI-DRPA0790-PVER-V-GUELTIG-AB  PIC X(026).
             10 PI-DRPA0790-PVER-V-GUELTIG-BIS PIC X(026).
             10 PI-DRPA0790-PVER-V-KENNUNG     PIC X(008).
             10 PI-DRPA0790-KNR-PVER-ART       PIC S9(4) USAGE COMP.
             10 PI-DRPA0790-PVER-VORVERTRAG    PIC X(001).
             10 PI-DRPA0790-PGEB-GES           PIC S9(4) USAGE COMP.
             10 PI-DRPA0790-PVER-VNR           PIC X(15).
             10 PI-DRPA0790-PVER-KUNDE-NAME    PIC X(25).
             10 PI-DRPA0790-PVER-KUNDE-VNAME   PIC X(20).
             10 PI-DRPA0790-PVER-VERTR-BEGINN  PIC X(10).
             10 PI-DRPA0790-PVER-VERTR-ABLAUF  PIC X(10).
             10 PI-DRPA0790-PVER-PG-VBNR       PIC S9(9) USAGE COMP.
             10 PI-DRPA0790-KNR-PVER-STAND     PIC S9(4) USAGE COMP.
             10 PI-DRPA0790-PVER-VSTAT         PIC  X(4).
             10 PI-DRPA0790-PVER-AKT-PIN       PIC S9(9) USAGE COMP.
             10 PI-DRPA0790-PVER-AVERM         PIC S9(9) USAGE COMP.
             10 PI-DRPA0790-PVER-VM            PIC S9(9) USAGE COMP.
             10 PI-DRPA0790-PVER-PNAP-EINTRAG  PIC X(1).
             10 PI-DRPA0790-PVER-AKT-DGV       PIC S9(4) USAGE COMP.
             10 PI-DRPA0790-PVER-AKT-PMBE-ID   PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0790-PVER-AKT-FAELLIG-V PIC X(10).
             10 PI-DRPA0790-PVER-AKT-FAELLIG-B PIC X(10).
             10 PI-DRPA0790-PVER-LAST-UPDATE   PIC X(26).
             10 PI-DRPA0790-PVER-KENNUNG       PIC X(08).
             10 PI-DRPA0790-KNR-MANDANT        PIC S9(4) USAGE COMP.
260713       10 PI-DRPA0790-PVER-BZ-NAME       PIC X(25).
260713       10 PI-DRPA0790-PVER-BZ-VNAME      PIC X(20).
260713       10 PI-DRPA0790-PVER-VP-NAME       PIC X(25).
260713       10 PI-DRPA0790-PVER-VP-VNAME      PIC X(20).
260713       10 PI-DRPA0790-KNR-FEIV-KZ        PIC S9(4) USAGE COMP.
260713       10 PI-DRPA0790-KNR-VERM-GES       PIC S9(4) USAGE COMP.
260713       10 PI-DRPA0790-PVER-UMW-NR        PIC S9(15)V USAGE COMP-3.
260713       10 PI-DRPA0790-PVER-URSPR-POL-DAT PIC X(10).
260713       10 PI-DRPA0790-PVER-VN-STRASSE    PIC X(35).
260713       10 PI-DRPA0790-PVER-VN-PLZ        PIC X(5).
260713       10 PI-DRPA0790-PVER-VN-ORT        PIC X(35).
130515       10 PI-DRPA0790-PVER-PG-UST-SATZ   PIC 9(03)V999.
200217       10 PI-DRPA0790-KNR-TLSN-PARTNER   PIC S9(4) USAGE COMP.
      *
       01 PO-DRPA0790.
          05 PO-DRPA0790-RC                    PIC  X(003).
             88 PO-DRPA0790-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0790-RC-DUPLICATE-VALUE       VALUE 'DVA'.
             88 PO-DRPA0790-RC-CHANGED               VALUE 'CHA'.
             88 PO-DRPA0790-RC-NOT-FOUND             VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
 

