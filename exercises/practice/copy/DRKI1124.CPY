      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1124.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1124.CPv  $
      *
      *   Rev 2.3   Sep 18 2012 15:13:42   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-KVB
      *
      *   Rev 2.2   Dec 18 2006 16:15:46   t13ex43
      *FKI00979: Korrektur mehrfach betreute Interessenten
      *
      *   Rev 2.1   18 Sep 2000 08:55:06   T13AE21
      *AKI00138
      *
      *   Rev 2.0   07 Jun 2000 17:08:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1124                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXAT0005                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PHILIPP                                          *
      *    DATUM:    11.09.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00138!11.09.00! 01 ! NEUERSTELLUNG             ! PHILIPP  *
141206* FKI00979!14.12.06! 2.2! LETZTEAENDERUNG HINZU     ! KLEEMEIER*
      * AKI00886!15.11.11!2.3 !Neue Kundenaktualisierung  !I.Frenzel *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0963                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1124.
          05 PI-DRKI1124-PROGRAMM                 PIC  X(008).
          05 PI-DRKI1124-UFKT-CODE                PIC  9(002).
          05 PI-DRKI1124-DATEN.
            10 PI-DRKI1124-KVBKIPERSONENID        PIC X(013).
      *
       01 PO-DRKI1124.

          05 PO-DRKI1124-RC                       PIC  X(003).
             88 PO-DRKI1124-RC-OK                    VALUE 'OK '.
             88 PO-DRKI1124-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRKI1124-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI1124-DATEN.
            10 PO-DRKI1124-KVBVBPERSONENID        PIC X(013).
141206      10 PO-DRKI1124-LETZTEAENDERUNG        PIC X(026).
      *
      ***** Ende Copy-Member DRKI0548 *****************************
