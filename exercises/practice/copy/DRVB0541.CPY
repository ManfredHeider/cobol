      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0541.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0541.CPv  $
      *
      *   Rev 2.2   Dec 10 2003 09:59:34   t13ae27
      *OLB-Auszahlung
      *
      *   Rev 2.1   17 May 2001 10:56:28   t13ae30
      * 
      *
      *   Rev 2.0   15 May 2001 13:33:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0541                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0541                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    15.05.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00274!15.05.01!    ! NEU                       !TH.WILLIG *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0541
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0541.
           05  PI-DRVB0541-MA-ST-NR           PIC S9(007) COMP.
           05  PI-DRVB0541-LIST-MONAT         PIC  X(010)     .
           05  PI-DRVB0541-KNR-WB-TYP         PIC S9(004) COMP.
           05  PI-DRVB0541-KNR-WERTUNGS-TYP   PIC S9(004) COMP.
      *
       01  PO-DRVB0541.
           05  PO-DRVB0541-RC                 PIC  X(003).
               88 PO-DRVB0541-RC-OK                  VALUE 'OK '.
               88 PO-DRVB0541-RC-NOK                 VALUE 'NOK'.
               88 PO-DRVB0541-RC-NOT-FOUND           VALUE 'NFD'.
               88 PO-DRVB0541-RC-ERR                 VALUE 'ERR'.
      *
           05  PO-DRVB0541-LIST-TUPEL-GESAMT  PIC X(268).
      *
      *********************** ENDE COPY-MEMBER *************************
