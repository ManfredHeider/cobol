      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0350.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0350.CPv  $
      *
      *   Rev 2.1   02 May 2001 08:58:36   t13ex49
      *AKI00161 - erstmalige Prod.-Übergabe
      *
      *   Rev 2.0   07 Dec 2000 09:36:10   p13sp16
      *Archiverstellung
      *
      ****************************************************************
      *
      *    COPY-MEMBER: DRAT0350                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPAT0030                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00161!03.01.01!    ! Neuerstellung             !Peter Pohl*
      *         !        !    ! Fehlerstatistik           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *--------------------------------------------------------------*
       01  DRAT0350.
           10  Fehlerstatistik-SATZ.
               20  FS-Fehler-SYSCODE           PIC X(02).
               20  FS-Fehler-Job-Name          PIC X(08).
               20  FS-Fehler-PGM               PIC X(08).
               20  FS-Fehler-GES               PIC 9(02).
               20  FS-Fehler-Nummer            PIC 9(04).
               20  FS-Fehler-Feldname          PIC X(15).
               20  FS-Fehler-Reaktion          PIC X(01).
               20  FS-Fehler-Relevanz          PIC X(01).
               20  FS-Fehler-Anzahl            PIC 9(04).
      *
      ******* ENDE COPY-MEMBER **************************************
