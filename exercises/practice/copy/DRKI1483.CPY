      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1483.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1483.CPv  $
      *
      *   Rev 2.1   Dec 12 2002 07:07:18   t13ex49
      *AKI00314 - erstmalige Prod-Übergabe
      *
      *   Rev 2.0   Oct 16 2002 08:32:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1483                                      *
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
      *    Satzaufbau Komplettbestand URL-Adressen
      *
      *    Länge: 165 Byte
      *
      ****************************************************************
      *
       01  DRKI1483.
           05 DRKI1483-STATUS                  PIC  X(001).
      *
           05 K-URL-KEY.
             10 DRKI1483-VBNUMMER              PIC  X(007).
      *
           05 DRKI1483-URL-NAME                PIC  X(070).
           05 DRKI1483-STUFE                   PIC  X(003).
           05 DRKI1483-PRAXIS                  PIC  X(001).
           05 DRKI1483-GESELLSCHAFT            PIC  X(001).
      *       1 DVAG  2 DVAG Wien

           05 DRKI1483-REGION                  PIC  X(004).
           05 DRKI1483-DIREKTION               PIC  X(004).
           05 DRKI1483-ADMI-CLUB               PIC  X(001).
           05 DRKI1483-AS-CLUB                 PIC  X(001).
           05 DRKI1483-BDV-Miglied             PIC  X(001).
           05 DRKI1483-VB-Beginn               PIC  X(010).
           05 DRKI1483-DL-Beginn               PIC  X(010).
           05 DRKI1483-ALD                     PIC  X(001).
           05 DRKI1483-LEER                    PIC  X(050).
      *
      *---------------------------------------------------------------*

