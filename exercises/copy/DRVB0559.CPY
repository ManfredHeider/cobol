      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0559.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRVB0559.CPv  $
      *
      *   Rev 2.1   Apr 30 2002 09:13:28   t13ae27
      *zusätzliche Parameter in CTV-Briefen
      *
      *   Rev 2.0   15 May 2001 13:33:38   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0559, ALLGEMEINE SCHNITTSTELLE           *
      *                 FÜR CTV-BATCH UPRO-CALL                      *
      *    --------------------------------------------------------  *
      *    ERSTELLT: P. Dürr                                         *
      *    DATUM:    17.10.2001                                      *
      ****************************************************************
      *    VERSION   1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00007!17.10.01!    ! NEU                       ! P.DÜRR   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   BESCHREIBUNG DER SCHNITTSTELLE:
      *
      *   EINGABE:
      *
      *     PI-DRVB0559-ST-NR
      *
      *   AUSGABE:
      *
      *     PO-DRVB0559-RC
      *     PO-DRVB0559-KUE-DAT.VOM     Kündigungsdatum  aus MAT
      *
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRVB0559.
           05  PI-DRVB0559-MA-ST-NR           PIC S9(009) COMP.
           05  PI-DRVB0559-CICS-DATUM         PIC X(010).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRVB0559.
           05  PO-DRVB0559-RC                 PIC  X(003).
               88  PO-DRVB0559-RC-OK                      VALUE 'OK '.
               88  PO-DRVB0559-RC-NOK                     VALUE 'NOK'.
               88  PO-DRVB0559-RC-NOT-FOUND               VALUE 'NFD'.

           05  PO-DRVB0559-KUE-DAT-VOM        PIC  X(010).

      ******** ENDE COPY-MEMBER ************************************

