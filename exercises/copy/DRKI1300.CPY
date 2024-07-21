      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1300.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/cpy/DRKI1300.CPv  $
      *
      *   Rev 2.2   20 Sep 2001 12:44:00   t13ex49
      *AKI00228- wg. fehlerhaftem NTPUT
      *
      *   Rev 2.1   20 Sep 2001 08:20:04   t13ex49
      *AKI00228 - Änderungen für Verwaltungs- und 
      *Vertriebsfond + Ausgabe der Direktionswechsel 
      *in Datei
      *
      *   Rev 2.0   18 Sep 2001 10:03:32   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1300                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI0944                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  28.10.99/19.23.01
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00228!15.08.01!    ! Ausgabe einer Datei mit   !Peter Pohl*
      *         !        !    ! den erkannten Direktions- !          *
      *         !        !    ! wechseln                  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    Dieses Copy enthält die Direktionswechsel eines Laufes der
      *    VB-Schnittstelle
      *
      *    Länge: 13 Byte
      *
      ****************************************************************

       01  DRKI1300.
           05 DIRW-VB-NR                   PIC 9(07).
           05 DIRW-DIR-NR-VON              PIC 9(03).
           05 DIRW-DIR-NR-NACH             PIC 9(03).
