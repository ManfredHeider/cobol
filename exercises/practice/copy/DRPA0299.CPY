      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0299.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0299.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:49:02   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:32:28   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0299                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LES. LETZTER EINTRAG>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    03.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.06.97! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!08.12.97! 1.1! ÄNDERUNG PDSP-DTA-ART IN  ! D.WILHELM*
      *         !        !    ! KNR-DTA-ART               !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0299                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0299.
          05 PI-DRPA0299-DATEN.
             10 PI-DRPA0299-PGEB-GES         PIC S9(004) BINARY.
081297       10 PI-DRPA0299-KNR-DTA-ART      PIC S9(004) BINARY.
      *
       01 PO-DRPA0299.
          05 PO-DRPA0299-RC                  PIC  X(003).
             88 PO-DRPA0299-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0299-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0299-DATEN.
             10 PO-DRPA0299-PDTA-ERSTELL-DAT PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
