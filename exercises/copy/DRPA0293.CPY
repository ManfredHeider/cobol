      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0293.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0293.CPv  $
      *
      *   Rev 2.1   May 28 1999 13:32:16   T13EX26
      *MFE die 2.
      *
      *   Rev 2.0   Feb 27 1999 14:28:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 09:27:14   T13EX16
      *APA00032 - PEW-Release 2.0
      *
      *   Rev 1.0   Apr 24 1997 14:31:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0293  FUNKTION "LESEN VSTATUS"           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    16.05.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!16.05.97! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0293                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0293.
          05 PI-DRPA0293-DATEN.
             10 PI-DRPA0293-UFKT                PIC  X(002).
      *
       01 PO-DRPA0293.
          05 PO-DRPA0293-RC                     PIC  X(003).
             88 PO-DRPA0293-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0293-RC-EOF                   VALUE 'EOF'.
             88 PO-DRPA0293-RC-LEER                  VALUE 'LER'.
          05 PO-DRPA0293-DATEN.
             10 PO-DRPA0293-VTAB-ZEILE.
                15 PO-DRPA0293-VSTAT            PIC  X(004).
                15 PO-DRPA0293-VSTAT-UNR        PIC S9(004) BINARY.
                15 PO-DRPA0293-ORT              PIC  X(030).
                15 PO-DRPA0293-BEREICH          PIC  X(010).
      *
      ******** ENDE COPY-MEMBER **************************************
