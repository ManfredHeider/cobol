      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0513.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0513.CPv  $
      *
      *   Rev 2.1   06 Nov 2000 12:55:10   T13AE30
      * 
      *
      *   Rev 2.0   07 Sep 2000 11:32:18   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0513                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0013                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    06.10.2000                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00048!06.11.00!2.1 ! NEUERSTELLUNG             ! TH.WILLIG*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0513:                                *
      *    ** WELCHER VB-VERTRAG ZUM STICHTAG JE VB                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0513.
           05  PI-DRVB0513-KEY.
               10 PI-DRVB0513-MA-ST-NR        PIC S9(009) BINARY.
               10 PI-DRVB0513-STICHTAG-ISO    PIC  X(010).
      *
       01  PO-DRVB0513.
           05  PO-DRVB0513-RC                 PIC  X(003).
               88 PO-DRVB0513-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0513-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0513-KNR-AV-TYP         PIC S9(004) BINARY.
      *
      ***************** ENDE COPY-MEMBER DRVB0513 *******************
