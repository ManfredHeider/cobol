      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1274.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRKI1274.CPv  $
      *
      *   Rev 2.1   Jan 27 2003 10:56:34   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 2.0   05 Jul 2001 17:07:12   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1274                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1274                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  24.07.01/14.43.12
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1274
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1274.
           05  PI-DRKI1274-KEY.


             10 PI-DRKI1274-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1274-GHPVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI1274.
           05  PO-DRKI1274-RC                 PIC  X(003).
               88 PO-DRKI1274-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1274-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1274-DATEN.


             10 PO-DRKI1274-GHPANLAGENAME.
                15 PO-DRKI1274-GHPANLAGENAME-LEN   PIC S9(4) COMP.
                15 PO-DRKI1274-GHPANLAGENAME-TEXT   PIC X(15).

             10 PO-DRKI1274-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1274-NI-GHPGUTHABEN   PIC S9(4) COMP.
             10 PO-DRKI1274-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PO-DRKI1274-NI-GHPGUTHABENANT   PIC S9(4) COMP.
             10 PO-DRKI1274-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1274-NI-GHPGUTHABENWERT   PIC S9(4) COMP.
             10 PO-DRKI1274-GHPGUTHABENWKZ   PIC X(01).
             10 PO-DRKI1274-GHPGUTHWERTWKZ   PIC X(01).
             10 PO-DRKI1274-GHPINFOSPERREDATUM   PIC X(10).
             10 PO-DRKI1274-GHPINFOWPERREKZ   PIC X(01).
             10 PO-DRKI1274-GHPPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI1274-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PO-DRKI1274-NI-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PO-DRKI1274-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1274-N-GHPSCHLSZGSCH   PIC S9(4) COMP.
             10 PO-DRKI1274-GHPSCHLAUSZGWKZ   PIC X(01).
             10 PO-DRKI1274-GHPSUBTYPE   PIC X(01).
             10 PO-DRKI1274-GHPVERFUEGUNGID   PIC X(2).
             10 PO-DRKI1274-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI1274-NI-GHPZINSSATZ   PIC S9(4) COMP.


      *
      ******* ENDE COPY-MEMBER **************************************
