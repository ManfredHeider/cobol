      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0266.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0266.cpv  $
      *
      *   Rev 2.1   Feb 14 2000 16:03:06   T13AE42
      *AVB00474 - Anpassung neuer VB-Vertrag
      *
      *   Rev 2.0   Feb 27 1999 15:58:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:24:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0266                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0266                    *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00474!11.02.00! 01 ! NEU                       !SCHMIDT,B.*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0266
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0266.
           05  PI-DRVB0266-KEY.
               10 PI-DRVB0266-MA-ST-NR        PIC S9(009) COMP.
      *
       01  PO-DRVB0266.
           05  PO-DRVB0266-RC                 PIC  X(003).
               88 PO-DRVB0266-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0266-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0266-AV-DAT-VON         PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************

