      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0563.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRVB0563.CPv  $
      *
      *   Rev 2.2   Apr 30 2002 09:14:00   t13ae27
      *zusätzliche Parameter in CTV-Briefen
      *
      *   Rev 2.1   Dec 18 2001 11:24:16   t13ae27
      *neue Funktion
      *
      *   Rev 2.0   15 May 2001 13:33:42   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0563                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0563                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. DUERR                                         *
      *    DATUM:    05.11.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00007!05.11.01!    ! NEU                       !P.DUERR   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0563
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0563.
           05  PI-DRVB0563-MA-ST-NR           PIC S9(009) COMP.
           05  PI-DRVB0563-MS-NR              PIC S9(004) COMP.
           05  PI-DRVB0563-BS-NR              PIC S9(004) COMP.
      *
       01  PO-DRVB0563.
           05  PO-DRVB0563-RC                 PIC  X(003).
               88 PO-DRVB0563-RC-OK                  VALUE 'OK '.
               88 PO-DRVB0563-RC-NOK                 VALUE 'NOK'.
               88 PO-DRVB0563-RC-NOT-FOUND           VALUE 'NFD'.
               88 PO-DRVB0563-RC-ERR                 VALUE 'ERR'.
      *
           05  PO-DRVB0563-MSBZ-BONUS         PIC 9(001).
           05  PO-DRVB0563-MSBZ-DAT-VON       PIC X(026).
           05  PO-DRVB0563-MSBZ-DAT-BIS       PIC X(010).
      *********************** ENDE COPY-MEMBER *************************


