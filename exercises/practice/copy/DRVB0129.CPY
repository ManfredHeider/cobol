      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0129.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0129.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:14:14   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0129                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0129                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GIESE                                            *
      *    DATUM:    12.09.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !12.09.96!    ! NEU                       ! GIESE    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0129
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0129.
           05  PI-DRVB0129-BRW-KEY.
               10 PI-DRVB0129-UNTST-MS-NR        PIC S9(004) BINARY.
               10 PI-DRVB0129-BETR-MS-NR         PIC S9(004) BINARY.
               10 PI-DRVB0129-UNTST-MS-BEZ-KURZ  PIC X(004).
               10 PI-DRVB0129-BETR-MS-BEZ-KURZ   PIC X(004).
      *
       01  PO-DRVB0129.
           05  PO-DRVB0129-RC                 PIC  X(003).
               88 PO-DRVB0129-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0129-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER DRVB0129 *****************************
