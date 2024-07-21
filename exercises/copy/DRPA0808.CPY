      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0808.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0808.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Aug 1998 09:35:46   T13EX16
      *APA00032 - PEW R2.0
      *
      *   Rev 1.0   14 Jan 1998 12:43:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0808                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0808                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    20.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !20.01.98!    ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0808
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    SELECT AUF DUMMY-TABELLE VIAT0006
      ****************************************************************
      *
       01  PI-DRPA0808                        PIC  X(001).
      *
       01  PO-DRPA0808.
           05  PO-DRPA0808-RC                 PIC  X(003).
               88 PO-DRPA0808-RC-OK                        VALUE 'OK '.
      *
           05  PO-DRPA0808-DATEN.
               10 PO-DRPA0808-ZEIT-COBOL       PIC  X(008).
               10 PO-DRPA0808-INDUSTRIETAGE    PIC  X(005).
      *
      ******* ENDE COPY-MEMBER **************************************
