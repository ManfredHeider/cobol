      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0964.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI0964.CPv  $
      *
      *   Rev 2.6   Sep 09 2015 08:47:58   i13az63
      *AAS01137: KLZ-Auswertung mit OZPs
      *
      *   Rev 2.5   Sep 08 2015 19:51:12   i13az63
      *AAS01137: KLZ-Auswertung mit OZPs
      *
      *   Rev 2.4   Sep 08 2015 15:06:14   i13az63
      *AAS01137: KLZ-Auswertung mit OZPs
      *
      *   Rev 2.3   22 Sep 2000 07:41:34   T13AE21
      *AKI00138
      *
      *   Rev 2.2   22 Sep 2000 07:34:10   T13AE21
      *AKI00138
      *
      *   Rev 2.1   09 Sep 2000 07:32:28   T13EX39
      *AKI00068 NEU
      *
      *   Rev 2.0   Feb 27 1999 12:58:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   25 Aug 1998 09:26:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0964                                     *
      *                                                              *
      *    FUNKTION: VSA-RELEVANTE DATEN AUFLISTEN                   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: P. Pohl                                         *
      *    DATUM:                                                    *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00068!10.01.00! 01 !NEU                        ! WHO      *
      * AKI00138!20.09.00!    !                           ! Philipp  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0964                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: VSA-RELEVANTE DATEN AUFLISTEN                   *
      *                                                              *
      ****************************************************************
       01 PI-DRKI0964.
          05  PI-DRKI0964-PROGRAMM                PIC X(008).
          05  PI-DRKI0964-UFKT-CODE               PIC X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05  PI-DRKI0964-DATEN.
              07 PI-DRKI0964-KVBVBPERSONENID             PIC X(13).
              07 PI-DRKI0964-KVBVBPERSONENROLLE          PIC X(01).
 
      ***
       01 PO-DRKI0964.
          05 PO-DRKI0964-RC                       PIC X(003).
             88 PO-DRKI0964-RC-OK                     VALUE 'OK '.
             88 PO-DRKI0964-RC-LEER                   VALUE 'LER'.
             88 PO-DRKI0964-RC-EOF                    VALUE 'EOF'.
             88 PO-DRKI0964-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRKI0964-DATEN.
             10 PO-DRKI0964-KVBKIPERSONENID             PIC X(13).
             10 PO-DRKI0964-KVBLOESCHKZ                 PIC X(01).
      ******** Ende Copy-Member **************************************
 
 

