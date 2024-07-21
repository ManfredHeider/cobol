      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $WORKFILE:   DRPA0716.CPY  $
      *    $REVISION:   2.0  $
      *
      *    $LOG:   L:\MF\CPY\DRPA0716.CPV  $
      *
      *   REV 2.0   MAY 26 1999 15:15:32   T13EX21
      *APA00012 - NEUERSTELLUNG
      *
      *   REV 1.0   12 DEC 1997 09:42:30   P13SP99
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0716  FUNKTION INSERT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    22.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.12.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      * APA00012!01.08.98! 1.1! BUCHUNGSGRUND & KZ-EINHEIT! PARENT   *
      *         !        !    !                           !          *
      * APA00012!24.08.98! 02 ! ERGÄNZT UM PDPE-DATEN (DN)! PARENT   *
      * APA01932!20.04.15! 2.1! UMSATZSTEUER EINGEFÜHRT   ! BUCH     *
      * APA02208!29.01.16! 2.2! STEUERJAHR ERGÄNZT        ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0716                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PDPT TBPA0122              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0716.
          05 PI-DRPA0716-PROGRAMM                 PIC  X(008).
          05 PI-DRPA0716-UFKT                     PIC  9(002).
          05 PI-DRPA0716-PDPT-DATEN.
             10 PI-DRPA0716-PDPT-ID               PIC S9(15)V COMP-3.
             10 PI-DRPA0716-PDPT-KENNUNG          PIC X(8).
             10 PI-DRPA0716-PMPT-ID               PIC S9(15)V COMP-3.
             10 PI-DRPA0716-PMPT-VERSION          PIC S9(4) COMP.
             10 PI-DRPA0716-PEST-ID               PIC S9(15)V COMP-3.
             10 PI-DRPA0716-PDPT-BUCH-RFOLGE      PIC S9(4) COMP.
             10 PI-DRPA0716-PDPT-MA-ABR-DAT       PIC X(10).
             10 PI-DRPA0716-KNR-BUCHUNGS-STAND    PIC S9(4) COMP.
             10 PI-DRPA0716-KNR-BUCHUNGSART       PIC S9(4) COMP.
150898       10 PI-DRPA0716-KNR-BUCHUNGS-GRUND    PIC S9(004) BINARY.
150898       10 PI-DRPA0716-KNR-KZ-EH-MAX         PIC S9(004) BINARY.
             10 PI-DRPA0716-KNR-BERCH-WAEHRUNG    PIC S9(4) COMP.
             10 PI-DRPA0716-PDPT-DGV              PIC S9(4) COMP.
             10 PI-DRPA0716-PDPT-PRODART          PIC S9(4) COMP.
240898       10 PI-DRPA0716-PDPT-PDPE-DATEN.
240898          15 PI-DRPA0716-PDPT-PDPE-DAT-LEN  PIC S9(4) USAGE COMP.
200415*         15 PI-DRPA0716-PDPT-PDPE-DAT-TEXT PIC X(720).
200415          15 PI-DRPA0716-PDPT-PDPE-DAT-TEXT PIC X(1220).
290116       10 PI-DRPA0716-PDPT-UST-JAHR         PIC S9(4) BINARY.
      *
       01 PO-DRPA0716.
          05 PO-DRPA0716-RC                       PIC  X(003).
             88 PO-DRPA0716-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0716-RC-DUPLICATE-VALUE            VALUE 'DVA'.
 
 

