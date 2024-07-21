      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0495.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0495.CPv  $
      *
      *   Rev 2.1   13 Oct 2000 09:36:04   T13AE28
      * 
      *
      *   Rev 2.0   19 May 2000 09:50:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0495                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0495                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    30.06.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00490!30.06.00! 01 ! NEU                       ! Walb     *
240322* VLA-4369!24.03.22! 02 ! Felddefinition COMP-3     ! U. Heim  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0495
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0495.
           05  PI-DRVB0495-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0495-DATUM-VON          PIC  X(010).
           05  PI-DRVB0495-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0495.
           05  PO-DRVB0495-RC                 PIC  X(003).
               88 PO-DRVB0495-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0495-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0495-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0495-RC-ERR                       VALUE 'ERR'.
      *
240322     05  PO-DRVB0495-EH-NEU-GS-EG       PIC  S9(010)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************


