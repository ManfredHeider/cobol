      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1482.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1482.CPv  $
      *
      *   Rev 2.1   Dec 12 2002 07:07:16   t13ex49
      *AKI00314 - erstmalige Prod-Übergabe
      *
      *   Rev 2.0   Oct 16 2002 08:31:56   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1482                                      *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00314!07.11.02!    ! NEU                       !Peter Pohl*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    Satzaufbau der Schnittstellendatei aus dem VB-System
      *
      *    Länge: 128 Byte
      *
      ****************************************************************

       01  DRKI1482.
           05 DRKI1482-VBNUMMER                PIC  9(007).
           05 DRKI1482-URL-NAME                PIC  X(070).
           05 DRKI1482-STUFE                   PIC  X(003).
           05 DRKI1482-PRAXIS                  PIC  X(001).
           05 DRKI1482-GESELLSCHAFT            PIC  X(001).
      *       1 DVAG  2 DVAG Wien

           05 DRKI1482-REGION                  PIC  X(004).
           05 DRKI1482-DIREKTION               PIC  X(004).
           05 DRKI1482-ADMI-CLUB               PIC  X(001).
           05 DRKI1482-AS-CLUB                 PIC  X(001).
           05 DRKI1482-BDV-Miglied             PIC  X(001).
           05 DRKI1482-VB-Beginn               PIC  X(010).
           05 DRKI1482-DL-Beginn               PIC  X(010).
           05 DRKI1482-ALD                     PIC  X(001).
           05 DRKI1482-LEER                    PIC  X(014).



      *---------------------------------------------------------------*

