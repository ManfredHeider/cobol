      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1249.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRKI1249.CPv  $
      *
      *   Rev 2.1   Jan 27 2003 10:56:08   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 2.0   05 Jul 2001 17:06:46   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1249                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1249                    *
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
      *    FšR DIE FUNKTION DRKI1249
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1249.
           05  PI-DRKI1249-KEY.


             10 PI-DRKI1249-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1249-GHPVERTRAGSID   PIC X(13).


           05  PI-DRKI1249-DATEN.


             10 PI-DRKI1249-GHPANLAGENAME.
                15 PI-DRKI1249-GHPANLAGENAME-LEN   PIC S9(4) COMP.
                15 PI-DRKI1249-GHPANLAGENAME-TEXT   PIC X(15).

             10 PI-DRKI1249-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1249-NI-GHPGUTHABEN   PIC S9(4) COMP.
             10 PI-DRKI1249-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PI-DRKI1249-NI-GHPGUTHABENANT   PIC S9(4) COMP.
             10 PI-DRKI1249-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1249-NI-GHPGUTHABENWERT   PIC S9(4) COMP.
             10 PI-DRKI1249-GHPGUTHABENWKZ   PIC X(01).
             10 PI-DRKI1249-GHPGUTHWERTWKZ   PIC X(01).
             10 PI-DRKI1249-GHPINFOSPERREDATUM   PIC X(10).
             10 PI-DRKI1249-GHPINFOWPERREKZ   PIC X(01).
             10 PI-DRKI1249-GHPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI1249-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PI-DRKI1249-NI-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PI-DRKI1249-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1249-N-GHPSCHLSZGSCH   PIC S9(4) COMP.
             10 PI-DRKI1249-GHPSCHLAUSZGWKZ   PIC X(01).
             10 PI-DRKI1249-GHPSUBTYPE   PIC X(01).
             10 PI-DRKI1249-GHPVERFUEGUNGID   PIC X(2).
             10 PI-DRKI1249-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1249-NI-GHPZINSSATZ   PIC S9(4) COMP.


      *
       01  PO-DRKI1249.
           05  PO-DRKI1249-RC                 PIC  X(003).
               88 PO-DRKI1249-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1249-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
