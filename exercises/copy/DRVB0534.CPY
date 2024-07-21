      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0534.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0534.CPv  $
      *
      *   Rev 2.1   09 May 2001 14:43:22   t13ae30
      * 
      *
      *   Rev 2.0   22 Feb 2001 16:59:54   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0534                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0534                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    09.05.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00534!09.05.01!    ! NEU                       !TH.WILLIG *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0534
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0534.
           05  PI-DRVB0534-MA-ST-NR           PIC  S9(009)    COMP.
      *
       01  PO-DRVB0534.
           05  PO-DRVB0534-RC                 PIC  X(003).
               88 PO-DRVB0534-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0534-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0534-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0534-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0534-MMPZ-TUPEL-GESAMT   PIC X(079).
      *
      *********************** ENDE COPY-MEMBER *************************
