      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1290.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1290.CPv  $
      *
      *   Rev 2.1   May 19 2004 16:15:12   t13ex39
      *Bestandsdatenerweiterung bei der AMV`ers
      *
      *   Rev 2.0   05 Jul 2001 17:07:26   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1290                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1290                    *
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
      *    FÜR DIE FUNKTION DRKI1290
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1290.
           05  PI-DRKI1290-KEY.


             10 PI-DRKI1290-SATVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1290-SATVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI1290.
           05  PO-DRKI1290-RC                 PIC  X(003).
               88 PO-DRKI1290-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1290-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1290-DATEN.


             10 PO-DRKI1290-SATGLASBERECHID   PIC S9(4) COMP.
             10 PO-DRKI1290-NI-SATGLASBERECHID   PIC S9(4) COMP.
             10 PO-DRKI1290-SATGLASBERECHWERT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1290-N-STGLSBRCHWRT   PIC S9(4) COMP.
             10 PO-DRKI1290-SATGLASBERWERTWKZ   PIC X(01).
             10 PO-DRKI1290-SATHINWAUFSELBSTB   PIC X(01).
             10 PO-DRKI1290-SATJAHRBEITRWKZ   PIC X(01).
             10 PO-DRKI1290-SATJAHRESBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1290-N-STJHRSBTRG   PIC S9(4) COMP.
             10 PO-DRKI1290-SATPRODUKTTEXTFV   PIC X(20).
             10 PO-DRKI1290-SATSEBSTBETWKZ   PIC X(01).
             10 PO-DRKI1290-SATSELBSTBETEIL   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1290-NI-SATSELBSTBETEIL   PIC S9(4) COMP.
             10 PO-DRKI1290-SATSUBTYPE   PIC X(01).
             10 PO-DRKI1290-SATVERSLSTID   PIC S9(4) COMP.
             10 PO-DRKI1290-NI-SATVERSLSTID   PIC S9(4) COMP.
             10 PO-DRKI1290-SATZWEIGSCHLUESSEL   PIC X(2).
             10 PO-DRKI1290-SATZWEIGTEXT   PIC X(3).
             

      *
      ******* ENDE COPY-MEMBER **************************************
