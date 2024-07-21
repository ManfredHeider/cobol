      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0261.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA0261.CPv  $
      *
      *   Rev 2.2   Mar 02 2021 10:39:28   t13ex44
      *Vorschüssige und VSP2021
      *
      *   Rev 2.1   Jan 18 2000 14:37:28   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Feb 27 1999 14:28:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:33:14   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:27:24   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0261                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PGV ERMITTELN       >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    09.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.06.97! 01 ! NEU                       ! WOLFF    *
      * APA00071!20.12.99! 02 ! PGV-ERMITTLUNG AUF TBPA0026 MUND     *
      *         !        !    ! UMGESTELLT                !          *
      * DSY03269!19.01.21!    ! VERARB-KZ IN PI-BEREICH   ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0261                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0261.
          05 PI-DRPA0261-DATEN.
             10 PI-DRPA0261-PGEB-GES         PIC S9(004)  BINARY.
             10 PI-DRPA0261-PGV-PG           PIC  9(004).
             10 PI-DRPA0261-VZ-POSITIV       PIC  X(001).
             10 PI-DRPA0261-STICHTAG         PIC  X(010).
190121       10 PI-DRPA0261-VERARB-KZ        PIC  9(004).
      *
       01 PO-DRPA0261.
          05 PO-DRPA0261-RC                  PIC  X(003).
             88 PO-DRPA0261-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0261-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0261-DATEN.
             10 PO-DRPA0261-PMPP-PGV         PIC S9(004)  BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
