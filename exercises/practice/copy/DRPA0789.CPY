      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0789.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRPA0789.CPv  $
      *
      *   Rev 2.8   Mar 28 2017 08:19:38   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs 
      *
      *   Rev 2.7   Jul 16 2015 15:08:20   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.6   Mar 10 2014 16:46:54   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.5   Jul 26 2013 11:41:54   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.4   Jun 19 2012 09:25:34   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:50:30   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 10:47:44   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   Apr 27 1999 13:47:08   T13EX26
      *V-GUELTIG-BIS auf DATE-Format geändert
      *
      *   Rev 2.0   Feb 27 1999 14:29:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:18:48   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   12 Dec 1997 09:47:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0789                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  VERTRAG EINFüGEN                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    10.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!10.01.98! 01 ! NEU                       ! STORCH   *
      * APA00012!31.07.98!    ! NEU: AVERM & VM           ! PARENT   *
      *         !        !    ! V-GUELT-BIS GUELT-AB      !          *
      *         !        !    ! V-KENNUNG, VORVERTRAG     !          *
      * APA00012!27.04.99! 2.1! V-GUELTIG-BIS X26 -> X10  ! PARENT   *
      * APA00622!23.05.07! 2.2! NEU: KNR-VGES             ! STORCH   *
      * APA01101!23.02.12! 2.3! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    ! *-BEREICHE ENTFERNT       !          *
260713* APA01539!26.07.13! 2.4! ERGÄNZUNGEN FÜR KALYPSO   ! RAHNER   *
      * APA01932!13.05.15! 2.7! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      * APA02353!21.02.17! 2.8! EINFÜHRUNG Teleson        ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0789                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0789.
          05 PI-DRPA0789-PROGRAMM              PIC  X(008).
          05 PI-DRPA0789-UFKT                  PIC  9(002).
          05 PI-DRPA0789-PVER-DATEN.
             10 PI-DRPA0789-PRINTE             PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0789-PVER-VERSION       PIC S9(4) USAGE COMP.
             10 PI-DRPA0789-PVER-F-GUELTIG-AB  PIC X(10).
             10 PI-DRPA0789-PVER-F-GUELTIG-BIS PIC X(10).
             10 PI-DRPA0789-PVER-V-GUELTIG-AB  PIC X(026).
             10 PI-DRPA0789-PVER-V-GUELTIG-BIS PIC X(010).
             10 PI-DRPA0789-PVER-V-KENNUNG     PIC X(008).
             10 PI-DRPA0789-KNR-PVER-ART       PIC S9(4) USAGE COMP.
             10 PI-DRPA0789-PVER-VORVERTRAG    PIC X(001).
             10 PI-DRPA0789-PGEB-GES           PIC S9(4) USAGE COMP.
             10 PI-DRPA0789-PVER-VNR           PIC X(15).
             10 PI-DRPA0789-PVER-KUNDE-NAME    PIC X(25).
             10 PI-DRPA0789-PVER-KUNDE-VNAME   PIC X(20).
             10 PI-DRPA0789-PVER-VERTR-BEGINN  PIC X(10).
             10 PI-DRPA0789-PVER-VERTR-ABLAUF  PIC X(10).
             10 PI-DRPA0789-PVER-PG-VBNR       PIC S9(9) USAGE COMP.
             10 PI-DRPA0789-KNR-PVER-STAND     PIC S9(4) USAGE COMP.
             10 PI-DRPA0789-PVER-VSTAT         PIC  X(4).
             10 PI-DRPA0789-PVER-AKT-PIN       PIC S9(9) USAGE COMP.
             10 PI-DRPA0789-PVER-AVERM         PIC S9(9) USAGE COMP.
             10 PI-DRPA0789-PVER-VM            PIC S9(9) USAGE COMP.
             10 PI-DRPA0789-PVER-PNAP-EINTRAG  PIC X(1).
             10 PI-DRPA0789-PVER-AKT-DGV       PIC S9(4) USAGE COMP.
             10 PI-DRPA0789-PVER-AKT-PMBE-ID   PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0789-PVER-AKT-FAELLIG-V PIC X(10).
             10 PI-DRPA0789-PVER-AKT-FAELLIG-B PIC X(10).
             10 PI-DRPA0789-KNR-MANDANT        PIC S9(4) USAGE COMP.
260713       10 PI-DRPA0789-PVER-BZ-NAME       PIC X(25).
260713       10 PI-DRPA0789-PVER-BZ-VNAME      PIC X(20).
260713       10 PI-DRPA0789-PVER-VP-NAME       PIC X(25).
260713       10 PI-DRPA0789-PVER-VP-VNAME      PIC X(20).
260713       10 PI-DRPA0789-KNR-FEIV-KZ        PIC S9(4) USAGE COMP.
260713       10 PI-DRPA0789-KNR-VERM-GES       PIC S9(4) USAGE COMP.
260713       10 PI-DRPA0789-PVER-UMW-NR        PIC S9(15)V USAGE COMP-3.
260713       10 PI-DRPA0789-PVER-URSPR-POL-DAT PIC X(10).
260713       10 PI-DRPA0789-PVER-VN-STRASSE    PIC X(35).
260713       10 PI-DRPA0789-PVER-VN-PLZ        PIC X(5).
260713       10 PI-DRPA0789-PVER-VN-ORT        PIC X(35).
130515       10 PI-DRPA0789-PVER-PG-UST-SATZ   PIC 9(03)V999.
210217       10 PI-DRPA0789-KNR-TLSN-PARTNER   PIC S9(4) USAGE COMP.
      *
       01 PO-DRPA0789.
          05 PO-DRPA0789-RC                     PIC  X(003).
             88 PO-DRPA0789-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0789-RC-DUPLICATE-VALUE       VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
 

