      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1192.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1192.CPv  $
      *
      *   Rev 2.2   Sep 30 2009 14:50:34   t13ex51
      *AKI00852: KI-Host - Kundensuche
      *
      *   Rev 2.1   01 Mar 2001 13:56:30   T13EX39
      *FKI00454 Komm. bei Haushalt
      *
      *   Rev 2.0   06 Feb 2001 09:41:42   p13sp16
      *Archiverstellung
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1192                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0043                        *
      *    Lesen PERSON mit Suchnamen, Suchvornamen und               *
      *    Geburtsdatum NULL.                                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WHO                                              *
      *    DATUM:    21.01.2001                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00454!21.02.01! 03 ! zus. Select für alle Haus-!WHO       *
      *         !        !    ! halte                     !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1192                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1192.
          05 PI-DRKI1192-PROGRAMM           PIC  X(008).
          05 PI-DRKI1192-UFKT-CODE          PIC  9(002).
          05 PI-DRKI1192-DATEN.
            07 PI-DRKI1192-personkundenart  PIC X(001).
      *
       01 PO-DRKI1192.

          05 PO-DRKI1192-RC                 PIC  X(003).
             88 PO-DRKI1192-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1192-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1192-RC-EOF                       VALUE 'EOF'.

          05 PO-DRKI1192-DATEN.
            07 PO-DRKI1192-personenid            PIC X(013).
      *
      ***** Ende Copy-Member DRKI1122 *****************************


