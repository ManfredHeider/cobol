      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1421.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/cpy/DRKI1421.CPv  $
      *
      *   Rev 2.2   Nov 06 2002 12:44:14   t13ex49
      *AKI00312 - neue Felder
      *
      *   Rev 2.1   Sep 02 2002 16:00:24   t13ex49
      *AKI00288 - eMail-Teil der 
      *VB-Schnittstelle 
      *erstmalige Übergabe
      *
      *   Rev 2.0   Apr 23 2002 16:02:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1421                                      *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00288!12.06.02!    ! NEU                       !Peter Pohl*
      * -------------------------------------------------------------*
      * AKI00312!23.10.02!    ! Feld SDNR-FLAG neu auf-   !Peter Pohl*
      *         !        !    ! genommen                  !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    Satzaufbau Delta der eMail-Adressen
      *
      *    Länge: 92 Byte
      *          115 Byte
      *
      ****************************************************************

       01  DRKI1421.
           05 DRKI1421-VERARB-KENNZEICHEN      PIC  X(001).
      *       A = Aktiv D = Delete

           05 DRKI1421-VBNUMMER                PIC  X(007).
           05 DRKI1421-EMAIL-ANSCHRIFT         PIC  X(080).
           05 DRKI1421-REGION                  PIC  X(004).
           05 DRKI1421-SDNR-FLAG               PIC  X(003).
           05 DRKI1421-LEERFELD                PIC  X(020).

      *---------------------------------------------------------------*

