      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0164.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0164.CPv  $
      *
      *   Rev 1.1   Jul 28 1999 13:32:26   T13EX16
      *APA00046 Neuerstellung für DAISY-PVW
      *
      *   Rev 1.0   05 Sep 1997 09:20:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0164                                      *
      *    --------------------------------------------------------   *
      *    KONSTANTEN MIT GEBROCHENEN ZAHLEN FüR DIE ALLGEMEINE       *
      *    UND PROJEKTSPEZIFISCH VERWENDUNG.                          *
      *                                                               *
      *    IN EINIGEN FäLLEN WERDEN KONSTANTEN MIT GEBROCHENEN        *
      *    ZAHLEN BENöTIGT Z.B. ZULäSSIGE ABWEICHUNG. IN ALLEN NEUEN  *
      *    PROGRAMME IST DIE OPTION 'DECIMAL POINT IS COMMA' AKTIV.   *
      *    EINIGEN ALTPROGRAMMEN WIRD DER PUNKT VERWENDET.            *
      *    UM PROBLEME ZU VERMEIDEN WERDEN DIE KONSTANTEN             *
      *    - IM DRAT0163 MIT KOMMA UND                                *
      *    - IM DRAT0164 MIT PUNKT DEFINIERT.                         *
      *    NEUE KONSTANTEN SIND IN BEIDEN COPIES EINZUTRAGEN.         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    20.10.1997                                       *
      *****************************************************************
      *    VERSION   1    08.06.95 16:19:49
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME     *
      * --------------------------------------------------------------*
      * AFPA0012!20.10.97!01.01! NEU                       ! AUERBACH *
      *         !        !     !                           !          *
      * AFPA0012!12.01.98!01.02! NEU: K-BEITRAG-ABWEICHUNG ! WOLFF    *
      *         !        !     !                           !          *
      * APA00012!16.11.98!01.03! Kosntante angepaßt        ! Jäger    *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************

      ***  KONSTANTEN FüR GEBROCHENE ZAHLEN
       01  FILLER.

      ***  DEFINIERTE GRUNDABWEICHUNG IN DAISY
161198*    05  K-DAISY-ABWEICHUNG         PIC  9(001)V9(2) VALUE 0.01.
161198     05  K-DAISY-ABWEICHUNG         PIC  9(001)V9(2) VALUE 0.05.
120198     05  K-BEITRAG-ABWEICHUNG       PIC  9(001)V9(2) VALUE 0.10.
      *
      ******** ENDE COPY-MEMBER STANDARD-KONSTANTEN (DRAT0164) ********
