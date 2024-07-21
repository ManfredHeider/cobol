      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1122.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1122.CPv  $
      *
      *   Rev 2.2   Sep 30 2009 14:50:26   t13ex51
      *AKI00852: KI-Host - Kundensuche
      *
      *   Rev 2.1   26 Sep 2000 17:17:44   T13EX12
      *AKI00141 - Neue Kundensuche
      *
      *   Rev 2.0   07 Jun 2000 17:08:14   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1122                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0043                        *
      *    Lesen PERSON mit Suchnamen, Suchvornamen und               *
      *    Geburtsdatum NULL.                                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: C. von der Goltz                                 *
      *    DATUM:    31.08.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00115!31.08.00! 01 ! NEUERSTELLUNG             !v.d.Goltz *
      * -------------------------------------------------------------*
      * AKI00852!31.08.09! 02 ! SUCHNAME AUF 30 STELLEN   ! HRO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1122                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1122.
          05 PI-DRKI1122-PROGRAMM           PIC  X(008).
          05 PI-DRKI1122-UFKT-CODE          PIC  9(002).
          05 PI-DRKI1122-DATEN.
310809*     07 PI-DRKI1122-suchname               PIC X(015).
310809*     07 PI-DRKI1122-suchvorname            PIC X(015).
310809      07 PI-DRKI1122-suchname               PIC X(030).
310809      07 PI-DRKI1122-suchvorname            PIC X(030).
            07 PI-DRKI1122-gebgruenddat           PIC X(010).
      *
       01 PO-DRKI1122.

          05 PO-DRKI1122-RC                 PIC  X(003).
             88 PO-DRKI1122-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1122-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1122-RC-EOF                       VALUE 'EOF'.

          05 PO-DRKI1122-DATEN.
            07 PO-DRKI1122-personenid            PIC X(013).
            07 PO-DRKI1122-gebgruenddat          PIC X(010).
      *
      ***** Ende Copy-Member DRKI1122 *****************************

