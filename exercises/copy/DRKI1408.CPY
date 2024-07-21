      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1408.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1408.CPv  $
      *
      *   Rev 2.2   May 12 2015 09:19:46   t13ex73
      *AKI01360: §34f VB-SST Umsetzung
      *
      *   Rev 2.1   Jun 13 2002 07:42:56   t13ex49
      *AKI00274 - NEU RDTADB24
      *
      *   Rev 2.0   Apr 09 2002 08:34:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1408                                      *
      *    --------------------------------------------------------   *
      *    VB-Schnittstelle -                                         *
      *    Datei für RDTADB24                                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Peter Pohl                                       *
      *    DATUM:    23. 04. 2002                                     *
      *****************************************************************
      *
      *    VERSION  1 - Länge 157 Byte
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00274!23.04.02!    ! NEU                       !Peter Pohl*
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI01360!23.04.15! 07 ! 34F Datum einfuegen       !DDY       *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      *--------------------------------------------------------------*
       01  DRKI1408.
           05 RDTA-VB-NR                   PIC 9(07).
           05 RDTA-Nachname-ALT            PIC X(30).
           05 RDTA-Nachname-NEU            PIC X(30).
           05 RDTA-Vorname-ALT             PIC X(15).
           05 RDTA-Vorname-NEU             PIC X(15).
           05 RDTA-GEBDAT-ALT              PIC X(10).
           05 RDTA-GEBDAT-NEU              PIC X(10).
           05 RDTA-Austritt-ALT            PIC X(10).
           05 RDTA-Austritt-NEU            PIC X(10).
           05 RDTA-34FDAT-ALT              PIC X(10).
           05 RDTA-34FDAT-NEU              PIC X(10).
      *---------------------------------------------------------------*

