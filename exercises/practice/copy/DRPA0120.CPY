      *--------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0120.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0120.CPv  $
      *
      *   Rev 1.1   Mar 06 1997 15:00:52   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0120  FUNKTION AVERM/VM ERMITTELN        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Pr”mpeler                                       *
      *    DATUM:    27.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !27.08.96! 01 ! Neuerstellung             ! Pr”mpeler*
      * --------!--------!----!---------------------------!----------*
      * DAISY 1 !22.01.97! 01 ! šBERARBEITUNG PEW-RELEASE ! WOLFF    *
      * PEW-Rel.!        !    !                           !          *
      * --------!--------!----!---------------------------!----------*
      * AFPA0002!04.03.97! 01 ! 'RC-STRUK-UNPLAU' IN      ! SCHŽFER  *
      *         !        !    ! 'RC-MEHRERE-VM-IN-S'      !          *
      *         !        !    ! GE„NDERT.                 !          *
      * --------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FšR DIE FUNKTION DRPA0120                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0120.
          05 PI-DRPA0120-PZVP-VERSION      PIC S9(004) BINARY.

       01 PO-DRPA0120.
          05 PO-DRPA0120-RC                PIC  X(003).
             88 PO-DRPA0120-RC-OK                      VALUE 'OK '.
040397*      88 PO-DRPA0120-RC-STRUK-UNPLAU            VALUE 'PLA'.
040397       88 PO-DRPA0120-RC-MEHRERE-VM-IN-S         VALUE 'MVM'.
             88 PO-DRPA0120-RC-KEIN-AVERM              VALUE 'KAV'.
             88 PO-DRPA0120-RC-AVERM-VTF               VALUE 'VTF'.
             88 PO-DRPA0120-RC-VTF-MIT-STRUK           VALUE 'VMS'.
          05 PO-DRPA0120-DATEN.
             10 PO-DRPA0120-AVERM          PIC S9(009) BINARY.
             10 PO-DRPA0120-VM             PIC S9(009) BINARY.
      *
      ******** Ende Copy-Member **************************************
