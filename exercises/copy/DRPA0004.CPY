      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0004.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drpa0004.cpv  $
      *
      *   Rev 2.2   Feb 17 2000 17:14:28   T13EX13
      *APA00081 - Pgeb-Ges in SQL-Statements
      *ergaenzt
      *
      *   Rev 2.1   Sep 02 1999 11:39:16   T13EX21
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:27:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Feb 19 1997 10:00:10   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0004  FUNKTION ERMITTELN ALLERN AB-      *
      *                           RECHENBAREN FOLGEFEHLER            *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    01.09.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * FPA00070!01.09.99!01.01! NEUERSTELLUNG             ! STORCH  *
      * APA00081!01.02.00!01.02!PGEB-GES IN SQL-STATEMENTS !WILKENS  *
      *         !        !     ! ERGAENZT                  !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0004                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0004.
          05 PI-DRPA0004-PROGRAMM            PIC  X(008).
          05 PI-DRPA0004-UFKT-CODE           PIC  9(002).
      *
       01 PO-DRPA0004.
          05 PO-DRPA0004-RC                PIC  X(003).
             88 PO-DRPA0004-RC-OK               VALUE 'OK '.
             88 PO-DRPA0004-RC-LEER             VALUE 'LER'.
             88 PO-DRPA0004-RC-EOF              VALUE 'EOF'.
          05 PO-DRPA0004-PNAP.
             10 PO-DRPA0004-PRINTE           PIC S9(015) PACKED-DECIMAL.
             10 PO-DRPA0004-PNAP-VNR         PIC  X(015).
             10 PO-DRPA0004-PNAP-FAELLIG-VON PIC  X(010).
             10 PO-DRPA0004-PNAP-PG-ABR-DAT  PIC  X(010).
             10 PO-DRPA0004-PNAP-ID          PIC S9(015) PACKED-DECIMAL.
             10 PO-DRPA0004-PNAP-VERSION     PIC S9(004) BINARY.
             10 PO-DRPA0004-PNAP-GSMT-PROV PIC S9(09)V99 PACKED-DECIMAL.
             10 PO-DRPA0004-PNAP-VSTAT       PIC  X(004).
010200       10 PO-DRPA0004-PGEB-GES         PIC S9(4) USAGE COMP.
      *
      ****** ENDE COPY-MEMBER ****************************************
