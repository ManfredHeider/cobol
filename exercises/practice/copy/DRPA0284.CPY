      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0284.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0284.CPv  $
      *
      *   Rev 2.1   Mar 27 2003 10:58:32   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.0   Feb 27 1999 14:28:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:44:36   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:30:40   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0284                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PGTA-TARIF  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    02.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.06.97!    ! NEU                       ! WOLFF    *
      * APA00225!21.03.03!2.01! ERWEITERUNG PG-TARIF      ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0284                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0284.
          05 PI-DRPA0284-PROGRAMM            PIC  X(008).
          05 PI-DRPA0284-UFKT                PIC  9(002).
          05 PI-DRPA0284-DATEN.
             10 PI-DRPA0284-PGEB-GES         PIC S9(004) BINARY.
210303*      10 PI-DRPA0284-PGTA-TARIF       PIC  X(012).
210303       10 PI-DRPA0284-PGTA-TARIF       PIC  X(015).
      *
       01 PO-DRPA0284.
          05 PO-DRPA0284-RC                  PIC  X(003).
             88 PO-DRPA0284-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0284-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0284-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0284-DATEN.
             10 PO-DRPA0284-PROD-PROD-SCHL   PIC S9(004)  BINARY.
             10 PO-DRPA0284-PGTA-GUELT-AB    PIC  X(010).
             10 PO-DRPA0284-PGTA-GUELT-BIS   PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
