      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  Januar 1997
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0222.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0222.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:53:58   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:11:28   P13PVCS
      *Initial revision.
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0222  AKTUELLE PROV. ERMITTELN FüR ALLE  *
      *                           STELLENTYPEN                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Giese                                           *
      *    DATUM:    10.02.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !10.02.97! 01 ! Neuerstellung             ! Giese    *
      *         !05.04.97!    ! Erweiterung Returncode    ! Giese    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0222                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *   MöGLICHE RETURNCODES DER FUNKTION DRPA0222                 *
      *   ------------------------------------------                 *
      *   PDN  PROVISION ZUM DATUM NICHT GEFUNDEN.                   *
      *   UFS  UNLOGISCHE FUNKTION FüR STELLENTYP                    *
      *        FüR STELLE GK UND SDNR SIND IN DEN ENTSPRECHENDEN     *
      *        TABELLEN KEINE PROVISIONEN VORGESEHEN                 *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0222.
          05 PI-DRPA0222-DATEN.
             10 PI-DRPA0222-ST-NR               PIC S9(09) BINARY.
             10 PI-DRPA0222-STICHTAG            PIC  X(10).

       01 PO-DRPA0222.
          05 PO-DRPA0222-RC                     PIC  X(003).
             88 PO-DRPA0222-RC-OK                    VALUE 'OK '.
050497       88 PO-DRPA0222-RC-PDN                   VALUE 'PDN'.
050497       88 PO-DRPA0222-RC-UFS                   VALUE 'UFS'.
          05 PO-DRPA0222-DIFF-PROV              PIC S9(03)V99 COMP-3.
          05 PO-DRPA0222-ST-TYP                 PIC S9(04).
      *
      ******** Ende Copy-Member **************************************
