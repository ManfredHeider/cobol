      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0564.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRVB0564.CPv  $
      *
      *   Rev 2.1   Apr 30 2002 09:14:18   t13ae27
      *zusätzliche Parameter in CTV-Briefen
      *
      *   Rev 2.0   15 May 2001 13:33:42   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0564                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0564                    *
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
      *    FÜR DIE FUNKTION DRVB0564
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0564.
           05  PI-DRVB0564-MA-ST-NR           PIC S9(009) COMP.
      *
       01  PO-DRVB0564.
           05  PO-DRVB0564-RC                 PIC  X(003).
               88 PO-DRVB0564-RC-OK                  VALUE 'OK '.
               88 PO-DRVB0564-RC-NOK                 VALUE 'NOK'.
               88 PO-DRVB0564-RC-NOT-FOUND           VALUE 'NFD'.
               88 PO-DRVB0564-RC-ERR                 VALUE 'ERR'.
      *
           05  PO-DRVB0564-VBP-NR             PIC S9(004) COMP.
           05  PO-DRVB0564-VBPZ-DAT-VON       PIC X(026).
           05  PO-DRVB0564-VBPZ-DAT-BIS       PIC X(010).
      *********************** ENDE COPY-MEMBER *************************

