      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1265.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1265.CPv  $
      *
      *   Rev 2.1   May 19 2004 16:14:42   t13ex39
      *Bestandsdatenerweiterung bei der AMV`ers
      *
      *   Rev 2.0   05 Jul 2001 17:07:02   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1265                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1265                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  19.05.2004/15:10:01
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1265
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1265.
           05  PI-DRKI1265-KEY.


             10 PI-DRKI1265-SATVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1265-SATVERTRAGSID   PIC X(13).
             

           05  PI-DRKI1265-DATEN.


             10 PI-DRKI1265-SATGLASBERECHID   PIC S9(4) COMP.
             10 PI-DRKI1265-NI-SATGLASBERECHID   PIC S9(4) COMP.
             10 PI-DRKI1265-SATGLASBERECHWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1265-N-STGLSBRCHWRT   PIC S9(4) COMP.
             10 PI-DRKI1265-SATGLASBERWERTWKZ   PIC X(01).
             10 PI-DRKI1265-SATHINWAUFSELBSTB   PIC X(01).
             10 PI-DRKI1265-SATJAHRBEITRWKZ   PIC X(01).
             10 PI-DRKI1265-SATJAHRESBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1265-N-STJHRSBTRG   PIC S9(4) COMP.
             10 PI-DRKI1265-SATPRODUKTTEXTFV   PIC X(20).
             10 PI-DRKI1265-SATSEBSTBETWKZ   PIC X(01).
             10 PI-DRKI1265-SATSELBSTBETEIL   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1265-NI-SATSELBSTBETEIL   PIC S9(4) COMP.
             10 PI-DRKI1265-SATSUBTYPE   PIC X(01).
             10 PI-DRKI1265-SATVERSLSTID   PIC S9(4) COMP.
             10 PI-DRKI1265-NI-SATVERSLSTID   PIC S9(4) COMP.
             10 PI-DRKI1265-SATZWEIGSCHLUESSEL   PIC X(2).
             10 PI-DRKI1265-SATZWEIGTEXT   PIC X(3).
             

      *
       01  PO-DRKI1265.
           05  PO-DRKI1265-RC                 PIC  X(003).
               88 PO-DRKI1265-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1265-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
