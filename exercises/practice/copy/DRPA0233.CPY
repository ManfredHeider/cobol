      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0233.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0233.CPv  $
      *
      *   Rev 2.0   May 26 1999 12:59:52   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:12:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0233                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0233                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    27.02.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0014!27.02.97!    ! NEU                       ! BOHLINGER*
      *         !14.08.97!    ! Anpassung aufgrund Tabel- ! STORCH   *
      *         !        !    ! lenänderungen             !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0233
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    JOIN AUF TABELLE PGEB UND KNR
      *                     (TBPA0008 UND TBAT0008)
      *    ERMITTELT FüR AKTUELLEN SATZ IN PGEB DIE ZUGEHöRIGEN BEZ10
      *    FüR WAEHRUNG EXPORT UND VERTRIEB-GES
      ****************************************************************
      *
       01  PI-DRPA0233.
           05  PI-DRPA0233-PROGRAMM            PIC  X(008).
bb    *    05  PI-DRPA0233-UFKT-CODE           PIC  X(02).
      *
           05  PI-DRPA0233-AUSWAHL-KRITERIEN.
               10 PI-DRPA0233-PGEB-GES           PIC S9(004) BINARY.
140897*        10 PI-DRPA0233-PGEB-GES-GUELT-AB  PIC  X(010).
140897         10 PI-DRPA0233-PGEB-GES-GUELT-AB  PIC  X(026).
      *
       01  PO-DRPA0233.
          05 PO-DRPA0233-RC                     PIC  X(003).
             88 PO-DRPA0233-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0233-RC-NOT-FOUND                  VALUE 'NFD'.
      *
           05  PO-DRPA0233-DATEN.
               10 PO-DRPA0233-KNR-WAEHRUNG       PIC S9(003) USAGE COMP.
               10 PO-DRPA0233-KTY-WAEHRUNG       PIC S9(003) USAGE COMP.
               10 PO-DRPA0233-KNR-EXPORT-ART     PIC S9(003) USAGE COMP.
               10 PO-DRPA0233-KTY-EXPORT-ART     PIC S9(003) USAGE COMP.
               10 PO-DRPA0233-KNR-VERTRIEB-GES   PIC S9(003) USAGE COMP.
               10 PO-DRPA0233-KTY-VERTRIEB-GES   PIC S9(003) USAGE COMP.
140897         10 PO-DRPA0233-KNR-BEWERT-ART     PIC S9(003) USAGE COMP.
140897         10 PO-DRPA0233-KTY-BEWERT-ART     PIC S9(003) USAGE COMP.
140897         10 PO-DRPA0233-KNR-AUSL           PIC S9(003) USAGE COMP.
140897         10 PO-DRPA0233-KTY-AUSL           PIC S9(003) USAGE COMP.
               10 PO-DRPA0233-PGEB-GES-GUELT-BIS PIC  X(010).
140897         10 PO-DRPA0233-PGEB-KENNUNG       PIC  X(008).
               10 PO-DRPA0233-PGEB-LAST-UPDATE   PIC  X(026).
               10 PO-DRPA0233-KNR-BEZ10-WAEHR    PIC  X(010).
               10 PO-DRPA0233-KNR-BEZ10-EXP-ART  PIC  X(010).
               10 PO-DRPA0233-KNR-BEZ10-VT-GES   PIC  X(010).
140897         10 PO-DRPA0233-KNR-BEZKURZ-AUSL   PIC  X(004).
140897         10 PO-DRPA0233-KNR-BEZ10-BEW-ART  PIC  X(010).
      ******* ENDE COPY-MEMBER **************************************
