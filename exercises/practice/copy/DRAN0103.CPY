      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0103.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0103.CPv  $
      *
      *   Rev 2.1   Apr 07 2005 09:52:26   t13ae34
      *AAN00429: vorhandene Produkte in LN-DB übertragen
      *
      *   Rev 2.0   Oct 01 2004 17:27:42   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0103                                     *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00429!04.10.04!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0039                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    ZUORDNUNG AML TARIFE+SPARTE ZU PRODUKTEN                  *
      *    -> KAPSELUNG INTERNE TABELLE UU300207                     *
      *                                                              *
      ****************************************************************
       01  TARTAB.
           05 FILLER            PIC X(13) VALUE '1NAF      201'.
           05 FILLER            PIC X(13) VALUE '1NAFH     201'.
           05 FILLER            PIC X(13) VALUE '1NAFI     201'.
           05 FILLER            PIC X(13) VALUE '1NAFIH    201'.
           05 FILLER            PIC X(13) VALUE '1NAFIR    201'.
           05 FILLER            PIC X(13) VALUE '1NAFIRH   201'.
           05 FILLER            PIC X(13) VALUE '1NAM      201'.
           05 FILLER            PIC X(13) VALUE '1NAMH     201'.
           05 FILLER            PIC X(13) VALUE '1NAMI     201'.
           05 FILLER            PIC X(13) VALUE '1NAMIH    201'.
           05 FILLER            PIC X(13) VALUE '1NAMIR    201'.
           05 FILLER            PIC X(13) VALUE '1NAMIRH   201'.
           05 FILLER            PIC X(13) VALUE 'BUF       435'.
           05 FILLER            PIC X(13) VALUE 'BUFH      435'.
           05 FILLER            PIC X(13) VALUE 'BUFD      435'.
           05 FILLER            PIC X(13) VALUE 'BUFHD     435'.
           05 FILLER            PIC X(13) VALUE 'BUA60F    435'.
           05 FILLER            PIC X(13) VALUE 'BUA60FH   435'.
           05 FILLER            PIC X(13) VALUE 'BUM       435'.
           05 FILLER            PIC X(13) VALUE 'BUMH      435'.
           05 FILLER            PIC X(13) VALUE 'BUMD      435'.
           05 FILLER            PIC X(13) VALUE 'BUMHD     435'.
           05 FILLER            PIC X(13) VALUE 'BUA60M    435'.
           05 FILLER            PIC X(13) VALUE 'BUA60MH   435'.
           05 FILLER            PIC X(13) VALUE 'BUA65M    435'.
           05 FILLER            PIC X(13) VALUE 'BUA65MH   435'.
           05 FILLER            PIC X(13) VALUE '1NF       201'.
           05 FILLER            PIC X(13) VALUE '1NFD      201'.
           05 FILLER            PIC X(13) VALUE '1NSF      211'.
           05 FILLER            PIC X(13) VALUE '1NFE      201'.
           05 FILLER            PIC X(13) VALUE '1NFEH     201'.
           05 FILLER            PIC X(13) VALUE '1NFH      201'.
           05 FILLER            PIC X(13) VALUE '1NFHD     201'.
           05 FILLER            PIC X(13) VALUE '1NSFH     211'.
           05 FILLER            PIC X(13) VALUE '1NFI      201'.
           05 FILLER            PIC X(13) VALUE '1NFID     201'.
           05 FILLER            PIC X(13) VALUE '1NFIH     201'.
           05 FILLER            PIC X(13) VALUE '1NFIHD    201'.
           05 FILLER            PIC X(13) VALUE '1NFIR     201'.
           05 FILLER            PIC X(13) VALUE '1NFIRD    201'.
           05 FILLER            PIC X(13) VALUE '1NFIRE    201'.
           05 FILLER            PIC X(13) VALUE '1NFIREH   201'.
           05 FILLER            PIC X(13) VALUE '1NFIRH    201'.
           05 FILLER            PIC X(13) VALUE '1NFIRHD   201'.
           05 FILLER            PIC X(13) VALUE '1NM       201'.
           05 FILLER            PIC X(13) VALUE '1NMD      201'.
           05 FILLER            PIC X(13) VALUE '1NSM      211'.
           05 FILLER            PIC X(13) VALUE '1NME      201'.
           05 FILLER            PIC X(13) VALUE '1NMEH     201'.
           05 FILLER            PIC X(13) VALUE '1NMH      201'.
           05 FILLER            PIC X(13) VALUE '1NMHD     201'.
           05 FILLER            PIC X(13) VALUE '1NSMH     211'.
           05 FILLER            PIC X(13) VALUE '1NMI      201'.
           05 FILLER            PIC X(13) VALUE '1NMID     201'.
           05 FILLER            PIC X(13) VALUE '1NMIH     201'.
           05 FILLER            PIC X(13) VALUE '1NMIHD    201'.
           05 FILLER            PIC X(13) VALUE '1NMIR     201'.
           05 FILLER            PIC X(13) VALUE '1NMIRD    201'.
           05 FILLER            PIC X(13) VALUE '1NMIRE    201'.
           05 FILLER            PIC X(13) VALUE '1NMIREH   201'.
           05 FILLER            PIC X(13) VALUE '1NMIRH    201'.
           05 FILLER            PIC X(13) VALUE '1NMIRHD   201'.
           05 FILLER            PIC X(13) VALUE '2N150F    202'.
           05 FILLER            PIC X(13) VALUE '2N150FD   202'.
           05 FILLER            PIC X(13) VALUE '2N150FE   202'.
           05 FILLER            PIC X(13) VALUE '2N150FEH  202'.
           05 FILLER            PIC X(13) VALUE '2N150FH   202'.
           05 FILLER            PIC X(13) VALUE '2N150FHD  202'.
           05 FILLER            PIC X(13) VALUE '2N150FI   202'.
           05 FILLER            PIC X(13) VALUE '2N150FID  202'.
           05 FILLER            PIC X(13) VALUE '2N150FIH  202'.
           05 FILLER            PIC X(13) VALUE '2N150FIHD 202'.
           05 FILLER            PIC X(13) VALUE '2N150FIR  202'.
           05 FILLER            PIC X(13) VALUE '2N150FIRD 202'.
           05 FILLER            PIC X(13) VALUE '2N150FIRE 202'.
           05 FILLER            PIC X(13) VALUE '2N150FIREH202'.
           05 FILLER            PIC X(13) VALUE '2N150FIRH 202'.
           05 FILLER            PIC X(13) VALUE '2N150FIRHD202'.
           05 FILLER            PIC X(13) VALUE '2N200F    202'.
           05 FILLER            PIC X(13) VALUE '2N200FD   202'.
           05 FILLER            PIC X(13) VALUE '2N200FE   202'.
           05 FILLER            PIC X(13) VALUE '2N200FEH  202'.
           05 FILLER            PIC X(13) VALUE '2N200FH   202'.
           05 FILLER            PIC X(13) VALUE '2N200FHD  202'.
           05 FILLER            PIC X(13) VALUE '2N200FI   202'.
           05 FILLER            PIC X(13) VALUE '2N200FID  202'.
           05 FILLER            PIC X(13) VALUE '2N200FIH  202'.
           05 FILLER            PIC X(13) VALUE '2N200FIHD 202'.
           05 FILLER            PIC X(13) VALUE '2N200FIR  202'.
           05 FILLER            PIC X(13) VALUE '2N200FIRD 202'.
           05 FILLER            PIC X(13) VALUE '2N200FIRE 202'.
           05 FILLER            PIC X(13) VALUE '2N200FIREH202'.
           05 FILLER            PIC X(13) VALUE '2N200FIRH 202'.
           05 FILLER            PIC X(13) VALUE '2N200FIRHD202'.
           05 FILLER            PIC X(13) VALUE '2N150M    202'.
           05 FILLER            PIC X(13) VALUE '2N150MD   202'.
           05 FILLER            PIC X(13) VALUE '2N150ME   202'.
           05 FILLER            PIC X(13) VALUE '2N150MEH  202'.
           05 FILLER            PIC X(13) VALUE '2N150MH   202'.
           05 FILLER            PIC X(13) VALUE '2N150MHD  202'.
           05 FILLER            PIC X(13) VALUE '2N150MI   202'.
           05 FILLER            PIC X(13) VALUE '2N150MID  202'.
           05 FILLER            PIC X(13) VALUE '2N150MIH  202'.
           05 FILLER            PIC X(13) VALUE '2N150MIHD 202'.
           05 FILLER            PIC X(13) VALUE '2N150MIR  202'.
           05 FILLER            PIC X(13) VALUE '2N150MIRD 202'.
           05 FILLER            PIC X(13) VALUE '2N150MIRE 202'.
           05 FILLER            PIC X(13) VALUE '2N150MIREH202'.
           05 FILLER            PIC X(13) VALUE '2N150MIRH 202'.
           05 FILLER            PIC X(13) VALUE '2N150MIRHD202'.
           05 FILLER            PIC X(13) VALUE '2N200M    202'.
           05 FILLER            PIC X(13) VALUE '2N200MD   202'.
           05 FILLER            PIC X(13) VALUE '2N200ME   202'.
           05 FILLER            PIC X(13) VALUE '2N200MEH  202'.
           05 FILLER            PIC X(13) VALUE '2N200MH   202'.
           05 FILLER            PIC X(13) VALUE '2N200MHD  202'.
           05 FILLER            PIC X(13) VALUE '2N200MI   202'.
           05 FILLER            PIC X(13) VALUE '2N200MID  202'.
           05 FILLER            PIC X(13) VALUE '2N200MIH  202'.
           05 FILLER            PIC X(13) VALUE '2N200MIHD 202'.
           05 FILLER            PIC X(13) VALUE '2N200MIR  202'.
           05 FILLER            PIC X(13) VALUE '2N200MIRD 202'.
           05 FILLER            PIC X(13) VALUE '2N200MIRE 202'.
           05 FILLER            PIC X(13) VALUE '2N200MIREH202'.
           05 FILLER            PIC X(13) VALUE '2N200MIRH 202'.
           05 FILLER            PIC X(13) VALUE '2N200MIRHD202'.
           05 FILLER            PIC X(13) VALUE '3NF       203'.
           05 FILLER            PIC X(13) VALUE '3NFD      203'.
           05 FILLER            PIC X(13) VALUE '3NFH      203'.
           05 FILLER            PIC X(13) VALUE '3NFHD     203'.
           05 FILLER            PIC X(13) VALUE '3NFI      203'.
           05 FILLER            PIC X(13) VALUE '3NFID     203'.
           05 FILLER            PIC X(13) VALUE '3NFIH     203'.
           05 FILLER            PIC X(13) VALUE '3NFIHD    203'.
           05 FILLER            PIC X(13) VALUE '3NM       203'.
           05 FILLER            PIC X(13) VALUE '3NMD      203'.
           05 FILLER            PIC X(13) VALUE '3NMH      203'.
           05 FILLER            PIC X(13) VALUE '3NMHD     203'.
           05 FILLER            PIC X(13) VALUE '3NMI      203'.
           05 FILLER            PIC X(13) VALUE '3NMID     203'.
           05 FILLER            PIC X(13) VALUE '3NMIH     203'.
           05 FILLER            PIC X(13) VALUE '3NMIHD    203'.
           05 FILLER            PIC X(13) VALUE '4NF       204'.
           05 FILLER            PIC X(13) VALUE '4NFD      204'.
           05 FILLER            PIC X(13) VALUE '4NFH      204'.
           05 FILLER            PIC X(13) VALUE '4NFHD     204'.
           05 FILLER            PIC X(13) VALUE '4NFI      204'.
           05 FILLER            PIC X(13) VALUE '4NFID     204'.
           05 FILLER            PIC X(13) VALUE '4NFIH     204'.
           05 FILLER            PIC X(13) VALUE '4NFIHD    204'.
           05 FILLER            PIC X(13) VALUE '4NFF      204'.
           05 FILLER            PIC X(13) VALUE '4NFFD     204'.
           05 FILLER            PIC X(13) VALUE '4NFFH     204'.
           05 FILLER            PIC X(13) VALUE '4NFFHD    204'.
           05 FILLER            PIC X(13) VALUE '4NFFI     204'.
           05 FILLER            PIC X(13) VALUE '4NFFID    204'.
           05 FILLER            PIC X(13) VALUE '4NFFIH    204'.
           05 FILLER            PIC X(13) VALUE '4NFFIHD   204'.
           05 FILLER            PIC X(13) VALUE '4NFFIR    204'.
           05 FILLER            PIC X(13) VALUE '4NFFIRD   204'.
           05 FILLER            PIC X(13) VALUE '4NFFIRH   204'.
           05 FILLER            PIC X(13) VALUE '4NFFIRHD  204'.
           05 FILLER            PIC X(13) VALUE '4NM       204'.
           05 FILLER            PIC X(13) VALUE '4NMD      204'.
           05 FILLER            PIC X(13) VALUE '4NMH      204'.
           05 FILLER            PIC X(13) VALUE '4NMHD     204'.
           05 FILLER            PIC X(13) VALUE '4NMI      204'.
           05 FILLER            PIC X(13) VALUE '4NMID     204'.
           05 FILLER            PIC X(13) VALUE '4NMIH     204'.
           05 FILLER            PIC X(13) VALUE '4NMIHD    204'.
           05 FILLER            PIC X(13) VALUE '4NMF      204'.
           05 FILLER            PIC X(13) VALUE '4NMFD     204'.
           05 FILLER            PIC X(13) VALUE '4NMFH     204'.
           05 FILLER            PIC X(13) VALUE '4NMFHD    204'.
           05 FILLER            PIC X(13) VALUE '4NMFI     204'.
           05 FILLER            PIC X(13) VALUE '4NMFID    204'.
           05 FILLER            PIC X(13) VALUE '4NMFIH    204'.
           05 FILLER            PIC X(13) VALUE '4NMFIHD   204'.
           05 FILLER            PIC X(13) VALUE '4NMFIR    204'.
           05 FILLER            PIC X(13) VALUE '4NMFIRD   204'.
           05 FILLER            PIC X(13) VALUE '4NMFIRH   204'.
           05 FILLER            PIC X(13) VALUE '4NMFIRHD  204'.
           05 FILLER            PIC X(13) VALUE '5NF       205'.
           05 FILLER            PIC X(13) VALUE '5NFD      205'.
           05 FILLER            PIC X(13) VALUE '5NFH      205'.
           05 FILLER            PIC X(13) VALUE '5NFHD     205'.
           05 FILLER            PIC X(13) VALUE '5NM       205'.
           05 FILLER            PIC X(13) VALUE '5NMD      205'.
           05 FILLER            PIC X(13) VALUE '5NMH      205'.
           05 FILLER            PIC X(13) VALUE '5NMHD     205'.
           05 FILLER            PIC X(13) VALUE '5NFM      205'.
           05 FILLER            PIC X(13) VALUE '5NFMD     205'.
           05 FILLER            PIC X(13) VALUE '5NFMH     205'.
           05 FILLER            PIC X(13) VALUE '5NFMHD    205'.
           05 FILLER            PIC X(13) VALUE '5NMF      205'.
           05 FILLER            PIC X(13) VALUE '5NMFD     205'.
           05 FILLER            PIC X(13) VALUE '5NMFH     205'.
           05 FILLER            PIC X(13) VALUE '5NMFHD    205'.
           05 FILLER            PIC X(13) VALUE '6NF       206'.
           05 FILLER            PIC X(13) VALUE '6NFD      206'.
           05 FILLER            PIC X(13) VALUE '6NFH      206'.
           05 FILLER            PIC X(13) VALUE '6NFHD     206'.
           05 FILLER            PIC X(13) VALUE '6NFI      206'.
           05 FILLER            PIC X(13) VALUE '6NFID     206'.
           05 FILLER            PIC X(13) VALUE '6NFIH     206'.
           05 FILLER            PIC X(13) VALUE '6NFIHD    206'.
           05 FILLER            PIC X(13) VALUE '6NM       206'.
           05 FILLER            PIC X(13) VALUE '6NMD      206'.
           05 FILLER            PIC X(13) VALUE '6NMH      206'.
           05 FILLER            PIC X(13) VALUE '6NMHD     206'.
           05 FILLER            PIC X(13) VALUE '6NMI      206'.
           05 FILLER            PIC X(13) VALUE '6NMID     206'.
           05 FILLER            PIC X(13) VALUE '6NMIH     206'.
           05 FILLER            PIC X(13) VALUE '6NMIHD    206'.
           05 FILLER            PIC X(13) VALUE '7N15SF    211'.
           05 FILLER            PIC X(13) VALUE '7N20SF    211'.
           05 FILLER            PIC X(13) VALUE '7N25SF    211'.
           05 FILLER            PIC X(13) VALUE '7N30SF    211'.
           05 FILLER            PIC X(13) VALUE '7N15SFH   211'.
           05 FILLER            PIC X(13) VALUE '7N20SFH   211'.
           05 FILLER            PIC X(13) VALUE '7N25SFH   211'.
           05 FILLER            PIC X(13) VALUE '7N30SFH   211'.
           05 FILLER            PIC X(13) VALUE '7N15SM    211'.
           05 FILLER            PIC X(13) VALUE '7N20SM    211'.
           05 FILLER            PIC X(13) VALUE '7N25SM    211'.
           05 FILLER            PIC X(13) VALUE '7N30SM    211'.
           05 FILLER            PIC X(13) VALUE '7N15SMH   211'.
           05 FILLER            PIC X(13) VALUE '7N20SMH   211'.
           05 FILLER            PIC X(13) VALUE '7N25SMH   211'.
           05 FILLER            PIC X(13) VALUE '7N30SMH   211'.
           05 FILLER            PIC X(13) VALUE '8N100F    208'.
           05 FILLER            PIC X(13) VALUE '8N100FD   208'.
           05 FILLER            PIC X(13) VALUE '8N100FH   208'.
           05 FILLER            PIC X(13) VALUE '8N100FHD  208'.
           05 FILLER            PIC X(13) VALUE '8N100FI   208'.
           05 FILLER            PIC X(13) VALUE '8N100FID  208'.
           05 FILLER            PIC X(13) VALUE '8N100FIH  208'.
           05 FILLER            PIC X(13) VALUE '8N100FIHD 208'.
           05 FILLER            PIC X(13) VALUE '8N100FIR  208'.
           05 FILLER            PIC X(13) VALUE '8N100FIRD 208'.
           05 FILLER            PIC X(13) VALUE '8N100FIRH 208'.
           05 FILLER            PIC X(13) VALUE '8N100FIRHD208'.
           05 FILLER            PIC X(13) VALUE '8N150F    208'.
           05 FILLER            PIC X(13) VALUE '8N150FD   208'.
           05 FILLER            PIC X(13) VALUE '8N150FH   208'.
           05 FILLER            PIC X(13) VALUE '8N150FHD  208'.
           05 FILLER            PIC X(13) VALUE '8N150FI   208'.
           05 FILLER            PIC X(13) VALUE '8N150FID  208'.
           05 FILLER            PIC X(13) VALUE '8N150FIH  208'.
           05 FILLER            PIC X(13) VALUE '8N150FIHD 208'.
           05 FILLER            PIC X(13) VALUE '8N150FIR  208'.
           05 FILLER            PIC X(13) VALUE '8N150FIRD 208'.
           05 FILLER            PIC X(13) VALUE '8N150FIRH 208'.
           05 FILLER            PIC X(13) VALUE '8N150FIRHD208'.
           05 FILLER            PIC X(13) VALUE '8N200F    208'.
           05 FILLER            PIC X(13) VALUE '8N200FD   208'.
           05 FILLER            PIC X(13) VALUE '8N200FH   208'.
           05 FILLER            PIC X(13) VALUE '8N200FHD  208'.
           05 FILLER            PIC X(13) VALUE '8N200FI   208'.
           05 FILLER            PIC X(13) VALUE '8N200FID  208'.
           05 FILLER            PIC X(13) VALUE '8N200FIH  208'.
           05 FILLER            PIC X(13) VALUE '8N200FIHD 208'.
           05 FILLER            PIC X(13) VALUE '8N200FIR  208'.
           05 FILLER            PIC X(13) VALUE '8N200FIRD 208'.
           05 FILLER            PIC X(13) VALUE '8N200FIRH 208'.
           05 FILLER            PIC X(13) VALUE '8N200FIRHD208'.
           05 FILLER            PIC X(13) VALUE '8N80F     208'.
           05 FILLER            PIC X(13) VALUE '8N80FD    208'.
           05 FILLER            PIC X(13) VALUE '8N80FH    208'.
           05 FILLER            PIC X(13) VALUE '8N80FHD   208'.
           05 FILLER            PIC X(13) VALUE '8N80FI    208'.
           05 FILLER            PIC X(13) VALUE '8N80FID   208'.
           05 FILLER            PIC X(13) VALUE '8N80FIH   208'.
           05 FILLER            PIC X(13) VALUE '8N80FIHD  208'.
           05 FILLER            PIC X(13) VALUE '8N80FIR   208'.
           05 FILLER            PIC X(13) VALUE '8N80FIRD  208'.
           05 FILLER            PIC X(13) VALUE '8N80FIRH  208'.
           05 FILLER            PIC X(13) VALUE '8N80FIRHD 208'.
           05 FILLER            PIC X(13) VALUE '8N100M    208'.
           05 FILLER            PIC X(13) VALUE '8N100MD   208'.
           05 FILLER            PIC X(13) VALUE '8N100MH   208'.
           05 FILLER            PIC X(13) VALUE '8N100MHD  208'.
           05 FILLER            PIC X(13) VALUE '8N100MI   208'.
           05 FILLER            PIC X(13) VALUE '8N100MID  208'.
           05 FILLER            PIC X(13) VALUE '8N100MIH  208'.
           05 FILLER            PIC X(13) VALUE '8N100MIHD 208'.
           05 FILLER            PIC X(13) VALUE '8N100MIR  208'.
           05 FILLER            PIC X(13) VALUE '8N100MIRD 208'.
           05 FILLER            PIC X(13) VALUE '8N100MIRH 208'.
           05 FILLER            PIC X(13) VALUE '8N100MIRHD208'.
           05 FILLER            PIC X(13) VALUE '8N150M    208'.
           05 FILLER            PIC X(13) VALUE '8N150MD   208'.
           05 FILLER            PIC X(13) VALUE '8N150MH   208'.
           05 FILLER            PIC X(13) VALUE '8N150MHD  208'.
           05 FILLER            PIC X(13) VALUE '8N150MI   208'.
           05 FILLER            PIC X(13) VALUE '8N150MID  208'.
           05 FILLER            PIC X(13) VALUE '8N150MIH  208'.
           05 FILLER            PIC X(13) VALUE '8N150MIHD 208'.
           05 FILLER            PIC X(13) VALUE '8N150MIR  208'.
           05 FILLER            PIC X(13) VALUE '8N150MIRD 208'.
           05 FILLER            PIC X(13) VALUE '8N150MIRH 208'.
           05 FILLER            PIC X(13) VALUE '8N150MIRHD208'.
           05 FILLER            PIC X(13) VALUE '8N200M    208'.
           05 FILLER            PIC X(13) VALUE '8N200MD   208'.
           05 FILLER            PIC X(13) VALUE '8N200MH   208'.
           05 FILLER            PIC X(13) VALUE '8N200MHD  208'.
           05 FILLER            PIC X(13) VALUE '8N200MI   208'.
           05 FILLER            PIC X(13) VALUE '8N200MID  208'.
           05 FILLER            PIC X(13) VALUE '8N200MIH  208'.
           05 FILLER            PIC X(13) VALUE '8N200MIHD 208'.
           05 FILLER            PIC X(13) VALUE '8N200MIR  208'.
           05 FILLER            PIC X(13) VALUE '8N200MIRD 208'.
           05 FILLER            PIC X(13) VALUE '8N200MIRH 208'.
           05 FILLER            PIC X(13) VALUE '8N200MIRHD208'.
           05 FILLER            PIC X(13) VALUE '8N80M     208'.
           05 FILLER            PIC X(13) VALUE '8N80MD    208'.
           05 FILLER            PIC X(13) VALUE '8N80MH    208'.
           05 FILLER            PIC X(13) VALUE '8N80MHD   208'.
           05 FILLER            PIC X(13) VALUE '8N80MI    208'.
           05 FILLER            PIC X(13) VALUE '8N80MID   208'.
           05 FILLER            PIC X(13) VALUE '8N80MIH   208'.
           05 FILLER            PIC X(13) VALUE '8N80MIHD  208'.
           05 FILLER            PIC X(13) VALUE '8N80MIR   208'.
           05 FILLER            PIC X(13) VALUE '8N80MIRD  208'.
           05 FILLER            PIC X(13) VALUE '8N80MIRH  208'.
           05 FILLER            PIC X(13) VALUE '8N80MIRHD 208'.
      *
           05 FILLER            PIC X(13) VALUE 'RNF       258'.
           05 FILLER            PIC X(13) VALUE 'RNFD      258'.
           05 FILLER            PIC X(13) VALUE 'RNFE      258'.
           05 FILLER            PIC X(13) VALUE 'RNFEH     258'.
           05 FILLER            PIC X(13) VALUE 'RNFH      258'.
           05 FILLER            PIC X(13) VALUE 'RNFHD     258'.
           05 FILLER            PIC X(13) VALUE 'RNFI      258'.
           05 FILLER            PIC X(13) VALUE 'RNFID     258'.
           05 FILLER            PIC X(13) VALUE 'RNFIH     258'.
           05 FILLER            PIC X(13) VALUE 'RNFUZV    258'.
           05 FILLER            PIC X(13) VALUE 'RNFIR     258'.
           05 FILLER            PIC X(13) VALUE 'RNFIRD    258'.
           05 FILLER            PIC X(13) VALUE 'RNFIRE    258'.
           05 FILLER            PIC X(13) VALUE 'RNFIREH   258'.
           05 FILLER            PIC X(13) VALUE 'RNFIRH    258'.
           05 FILLER            PIC X(13) VALUE 'RNFIRHD   258'.
           05 FILLER            PIC X(13) VALUE 'RSF       258'.
           05 FILLER            PIC X(13) VALUE 'RSFH      258'.
           05 FILLER            PIC X(13) VALUE 'RSFE      258'.
           05 FILLER            PIC X(13) VALUE 'RSFEH     258'.
           05 FILLER            PIC X(13) VALUE 'RSFI      258'.
           05 FILLER            PIC X(13) VALUE 'RSFIH     258'.
           05 FILLER            PIC X(13) VALUE 'RSFIR     258'.
           05 FILLER            PIC X(13) VALUE 'RSFIRE    258'.
           05 FILLER            PIC X(13) VALUE 'RSFIREH   258'.
           05 FILLER            PIC X(13) VALUE 'RSFIRH    258'.
           05 FILLER            PIC X(13) VALUE 'RNM       258'.
           05 FILLER            PIC X(13) VALUE 'RNMD      258'.
           05 FILLER            PIC X(13) VALUE 'RNME      258'.
           05 FILLER            PIC X(13) VALUE 'RNMEH     258'.
           05 FILLER            PIC X(13) VALUE 'RNMH      258'.
           05 FILLER            PIC X(13) VALUE 'RNMHD     258'.
           05 FILLER            PIC X(13) VALUE 'RNMI      258'.
           05 FILLER            PIC X(13) VALUE 'RNMID     258'.
           05 FILLER            PIC X(13) VALUE 'RNMIH     258'.
           05 FILLER            PIC X(13) VALUE 'RNMUZV    258'.
           05 FILLER            PIC X(13) VALUE 'RNMIR     258'.
           05 FILLER            PIC X(13) VALUE 'RNMIRD    258'.
           05 FILLER            PIC X(13) VALUE 'RNMIRE    258'.
           05 FILLER            PIC X(13) VALUE 'RNMIREH   258'.
           05 FILLER            PIC X(13) VALUE 'RNMIRH    258'.
           05 FILLER            PIC X(13) VALUE 'RNMIRHD   258'.
           05 FILLER            PIC X(13) VALUE 'RSM       258'.
           05 FILLER            PIC X(13) VALUE 'RSMH      258'.
           05 FILLER            PIC X(13) VALUE 'RSME      258'.
           05 FILLER            PIC X(13) VALUE 'RSMEH     258'.
           05 FILLER            PIC X(13) VALUE 'RSMI      258'.
           05 FILLER            PIC X(13) VALUE 'RSMIH     258'.
           05 FILLER            PIC X(13) VALUE 'RSMIR     258'.
           05 FILLER            PIC X(13) VALUE 'RSMIRE    258'.
           05 FILLER            PIC X(13) VALUE 'RSMIREH   258'.
           05 FILLER            PIC X(13) VALUE 'RSMIRH    258'.
      *
190303*>>>>>
           05 FILLER            PIC X(13) VALUE 'RNPF      258'.
           05 FILLER            PIC X(13) VALUE 'RNPFD     258'.
           05 FILLER            PIC X(13) VALUE 'RNPFE     258'.
           05 FILLER            PIC X(13) VALUE 'RNPFEH    258'.
           05 FILLER            PIC X(13) VALUE 'RNPFH     258'.
           05 FILLER            PIC X(13) VALUE 'RNPFHD    258'.
           05 FILLER            PIC X(13) VALUE 'RNPFI     258'.
           05 FILLER            PIC X(13) VALUE 'RNPFID    258'.
           05 FILLER            PIC X(13) VALUE 'RNPFIH    258'.
           05 FILLER            PIC X(13) VALUE 'RNPFUZV   258'.
           05 FILLER            PIC X(13) VALUE 'RNPFIR    258'.
           05 FILLER            PIC X(13) VALUE 'RNPFIRD   258'.
           05 FILLER            PIC X(13) VALUE 'RNPFIRE   258'.
           05 FILLER            PIC X(13) VALUE 'RNPFIREH  258'.
           05 FILLER            PIC X(13) VALUE 'RNPFIRH   258'.
           05 FILLER            PIC X(13) VALUE 'RNPFIRHD  258'.
           05 FILLER            PIC X(13) VALUE 'RSPF      258'.
           05 FILLER            PIC X(13) VALUE 'RSPFH     258'.
           05 FILLER            PIC X(13) VALUE 'RSPFE     258'.
           05 FILLER            PIC X(13) VALUE 'RSPFEH    258'.
           05 FILLER            PIC X(13) VALUE 'RSPFI     258'.
           05 FILLER            PIC X(13) VALUE 'RSPFIH    258'.
           05 FILLER            PIC X(13) VALUE 'RSPFIR    258'.
           05 FILLER            PIC X(13) VALUE 'RSPFIRE   258'.
           05 FILLER            PIC X(13) VALUE 'RSPFIREH  258'.
           05 FILLER            PIC X(13) VALUE 'RSPFIRH   258'.

           05 FILLER            PIC X(13) VALUE 'RNPFM     258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMD    258'.
           05 FILLER            PIC X(13) VALUE 'RNPFME    258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMEH   258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMH    258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMHD   258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMI    258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMID   258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMIH   258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMUZV  258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMIR   258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMIRD  258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMIRE  258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMIREH 258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMIRH  258'.
           05 FILLER            PIC X(13) VALUE 'RNPFMIRHD 258'.
           05 FILLER            PIC X(13) VALUE 'RSPFM     258'.
           05 FILLER            PIC X(13) VALUE 'RSPFMH    258'.
           05 FILLER            PIC X(13) VALUE 'RSPFME    258'.
           05 FILLER            PIC X(13) VALUE 'RSPFMEH   258'.
           05 FILLER            PIC X(13) VALUE 'RSPFMI    258'.
           05 FILLER            PIC X(13) VALUE 'RSPFMIH   258'.
           05 FILLER            PIC X(13) VALUE 'RSPFMIR   258'.
           05 FILLER            PIC X(13) VALUE 'RSPFMIRE  258'.
           05 FILLER            PIC X(13) VALUE 'RSPFMIREH 258'.
           05 FILLER            PIC X(13) VALUE 'RSPFMIRH  258'.

           05 FILLER            PIC X(13) VALUE 'RNPMF     258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFD    258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFE    258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFEH   258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFH    258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFHD   258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFI    258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFID   258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFIH   258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFUZV  258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFIR   258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFIRD  258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFIRE  258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFIREH 258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFIRH  258'.
           05 FILLER            PIC X(13) VALUE 'RNPMFIRHD 258'.
           05 FILLER            PIC X(13) VALUE 'RSPMF     258'.
           05 FILLER            PIC X(13) VALUE 'RSPMFH    258'.
           05 FILLER            PIC X(13) VALUE 'RSPMFE    258'.
           05 FILLER            PIC X(13) VALUE 'RSPMFEH   258'.
           05 FILLER            PIC X(13) VALUE 'RSPMFI    258'.
           05 FILLER            PIC X(13) VALUE 'RSPMFIH   258'.
           05 FILLER            PIC X(13) VALUE 'RSPMFIR   258'.
           05 FILLER            PIC X(13) VALUE 'RSPMFIRE  258'.
           05 FILLER            PIC X(13) VALUE 'RSPMFIREH 258'.
           05 FILLER            PIC X(13) VALUE 'RSPMFIRH  258'.

           05 FILLER            PIC X(13) VALUE 'RFPF      258'.
           05 FILLER            PIC X(13) VALUE 'RFPFH     258'.
           05 FILLER            PIC X(13) VALUE 'RFPFE     258'.
           05 FILLER            PIC X(13) VALUE 'RFPFEH    258'.
           05 FILLER            PIC X(13) VALUE 'RFPFI     258'.
           05 FILLER            PIC X(13) VALUE 'RFPFIH    258'.
           05 FILLER            PIC X(13) VALUE 'RFPFIR    258'.
           05 FILLER            PIC X(13) VALUE 'RFPFIRE   258'.
           05 FILLER            PIC X(13) VALUE 'RFPFIREH  258'.
           05 FILLER            PIC X(13) VALUE 'RFPFIRH   258'.

           05 FILLER            PIC X(13) VALUE 'RFF       258'.
           05 FILLER            PIC X(13) VALUE 'RFFH      258'.
           05 FILLER            PIC X(13) VALUE 'RFFE      258'.
           05 FILLER            PIC X(13) VALUE 'RFFEH     258'.
           05 FILLER            PIC X(13) VALUE 'RFFI      258'.
           05 FILLER            PIC X(13) VALUE 'RFFIH     258'.
           05 FILLER            PIC X(13) VALUE 'RFFIR     258'.
           05 FILLER            PIC X(13) VALUE 'RFFIRE    258'.
           05 FILLER            PIC X(13) VALUE 'RFFIREH   258'.
           05 FILLER            PIC X(13) VALUE 'RFFIRH    258'.

           05 FILLER            PIC X(13) VALUE 'RFPFM     258'.
           05 FILLER            PIC X(13) VALUE 'RFPFMH    258'.
           05 FILLER            PIC X(13) VALUE 'RFPFME    258'.
           05 FILLER            PIC X(13) VALUE 'RFPFMEH   258'.
           05 FILLER            PIC X(13) VALUE 'RFPFMI    258'.
           05 FILLER            PIC X(13) VALUE 'RFPFMIH   258'.
           05 FILLER            PIC X(13) VALUE 'RFPFMIR   258'.
           05 FILLER            PIC X(13) VALUE 'RFPFMIRE  258'.
           05 FILLER            PIC X(13) VALUE 'RFPFMIREH 258'.
           05 FILLER            PIC X(13) VALUE 'RFPFMIRH  258'.

           05 FILLER            PIC X(13) VALUE 'RFM       258'.
           05 FILLER            PIC X(13) VALUE 'RFMH      258'.
           05 FILLER            PIC X(13) VALUE 'RFME      258'.
           05 FILLER            PIC X(13) VALUE 'RFMEH     258'.
           05 FILLER            PIC X(13) VALUE 'RFMI      258'.
           05 FILLER            PIC X(13) VALUE 'RFMIH     258'.
           05 FILLER            PIC X(13) VALUE 'RFMIR     258'.
           05 FILLER            PIC X(13) VALUE 'RFMIRE    258'.
           05 FILLER            PIC X(13) VALUE 'RFMIREH   258'.
           05 FILLER            PIC X(13) VALUE 'RFMIRH    258'.

           05 FILLER            PIC X(13) VALUE 'RNPM      258'.
           05 FILLER            PIC X(13) VALUE 'RNPMD     258'.
           05 FILLER            PIC X(13) VALUE 'RNPME     258'.
           05 FILLER            PIC X(13) VALUE 'RNPMEH    258'.
           05 FILLER            PIC X(13) VALUE 'RNPMH     258'.
           05 FILLER            PIC X(13) VALUE 'RNPMHD    258'.
           05 FILLER            PIC X(13) VALUE 'RNPMI     258'.
           05 FILLER            PIC X(13) VALUE 'RNPMID    258'.
           05 FILLER            PIC X(13) VALUE 'RNPMIH    258'.
           05 FILLER            PIC X(13) VALUE 'RNPMUZV   258'.
           05 FILLER            PIC X(13) VALUE 'RNPMIR    258'.
           05 FILLER            PIC X(13) VALUE 'RNPMIRD   258'.
           05 FILLER            PIC X(13) VALUE 'RNPMIRE   258'.
           05 FILLER            PIC X(13) VALUE 'RNPMIREH  258'.
           05 FILLER            PIC X(13) VALUE 'RNPMIRH   258'.
           05 FILLER            PIC X(13) VALUE 'RNPMIRHD  258'.
           05 FILLER            PIC X(13) VALUE 'RSPM      258'.
           05 FILLER            PIC X(13) VALUE 'RSPMH     258'.
           05 FILLER            PIC X(13) VALUE 'RSPME     258'.
           05 FILLER            PIC X(13) VALUE 'RSPMEH    258'.
           05 FILLER            PIC X(13) VALUE 'RSPMI     258'.
           05 FILLER            PIC X(13) VALUE 'RSPMIH    258'.
           05 FILLER            PIC X(13) VALUE 'RSPMIR    258'.
           05 FILLER            PIC X(13) VALUE 'RSPMIRE   258'.
           05 FILLER            PIC X(13) VALUE 'RSPMIREH  258'.
           05 FILLER            PIC X(13) VALUE 'RSPMIRH   258'.

           05 FILLER            PIC X(13) VALUE 'RFPM      258'.
           05 FILLER            PIC X(13) VALUE 'RFPMH     258'.
           05 FILLER            PIC X(13) VALUE 'RFPME     258'.
           05 FILLER            PIC X(13) VALUE 'RFPMEH    258'.
           05 FILLER            PIC X(13) VALUE 'RFPMI     258'.
           05 FILLER            PIC X(13) VALUE 'RFPMIH    258'.
           05 FILLER            PIC X(13) VALUE 'RFPMIR    258'.
           05 FILLER            PIC X(13) VALUE 'RFPMIRE   258'.
           05 FILLER            PIC X(13) VALUE 'RFPMIREH  258'.
           05 FILLER            PIC X(13) VALUE 'RFPMIRH   258'.

           05 FILLER            PIC X(13) VALUE 'RFPMF     258'.
           05 FILLER            PIC X(13) VALUE 'RFPMFH    258'.
           05 FILLER            PIC X(13) VALUE 'RFPMFE    258'.
           05 FILLER            PIC X(13) VALUE 'RFPMFEH   258'.
           05 FILLER            PIC X(13) VALUE 'RFPMFI    258'.
           05 FILLER            PIC X(13) VALUE 'RFPMFIH   258'.
           05 FILLER            PIC X(13) VALUE 'RFPMFIR   258'.
           05 FILLER            PIC X(13) VALUE 'RFPMFIRE  258'.
           05 FILLER            PIC X(13) VALUE 'RFPMFIREH 258'.
           05 FILLER            PIC X(13) VALUE 'RFPMFIRH  258'.
190303*<<<<<
           05 FILLER            PIC X(13) VALUE 'RE        200'.
           05 FILLER            PIC X(13) VALUE 'REC       200'.
           05 FILLER            PIC X(13) VALUE 'REH       200'.
           05 FILLER            PIC X(13) VALUE '1FF       360'.
           05 FILLER            PIC X(13) VALUE '1FFE      360'.
           05 FILLER            PIC X(13) VALUE '1NF       360'.
           05 FILLER            PIC X(13) VALUE '1NFE      360'.
           05 FILLER            PIC X(13) VALUE '1NFI      360'.
           05 FILLER            PIC X(13) VALUE '1NFIR     360'.
           05 FILLER            PIC X(13) VALUE '1NFIRE    360'.
           05 FILLER            PIC X(13) VALUE '1NFH      360'.
           05 FILLER            PIC X(13) VALUE '1NFEH     360'.
           05 FILLER            PIC X(13) VALUE '1NFIH     360'.
           05 FILLER            PIC X(13) VALUE '1NFIRH    360'.
           05 FILLER            PIC X(13) VALUE '1NFIREH   360'.
           05 FILLER            PIC X(13) VALUE '1FM       360'.
           05 FILLER            PIC X(13) VALUE '1FME      360'.
           05 FILLER            PIC X(13) VALUE '1F160M    360'.
           05 FILLER            PIC X(13) VALUE '1NM       360'.
           05 FILLER            PIC X(13) VALUE '1NME      360'.
           05 FILLER            PIC X(13) VALUE '1NMI      360'.
           05 FILLER            PIC X(13) VALUE '1NMIR     360'.
           05 FILLER            PIC X(13) VALUE '1NMIRE    360'.
           05 FILLER            PIC X(13) VALUE '1NMH      360'.
           05 FILLER            PIC X(13) VALUE '1NMEH     360'.
           05 FILLER            PIC X(13) VALUE '1NMIH     360'.
           05 FILLER            PIC X(13) VALUE '1NMIRH    360'.
           05 FILLER            PIC X(13) VALUE '1NMIREH   360'.
           05 FILLER            PIC X(13) VALUE '2F150F    360'.
           05 FILLER            PIC X(13) VALUE '2F150FE   360'.
           05 FILLER            PIC X(13) VALUE '2F200F    360'.
           05 FILLER            PIC X(13) VALUE '2F200FE   360'.
           05 FILLER            PIC X(13) VALUE '2N150F    360'.
           05 FILLER            PIC X(13) VALUE '2N150FE   360'.
           05 FILLER            PIC X(13) VALUE '2N150FH   360'.
           05 FILLER            PIC X(13) VALUE '2N150FEH  360'.
           05 FILLER            PIC X(13) VALUE '2N150FIH  360'.
           05 FILLER            PIC X(13) VALUE '2N150FIRH 360'.
           05 FILLER            PIC X(13) VALUE '2N150FIREH360'.
           05 FILLER            PIC X(13) VALUE '2N150FI   360'.
           05 FILLER            PIC X(13) VALUE '2N150FIR  360'.
           05 FILLER            PIC X(13) VALUE '2N150FIRE 360'.
           05 FILLER            PIC X(13) VALUE '2N200F    360'.
           05 FILLER            PIC X(13) VALUE '2N200FE   360'.
           05 FILLER            PIC X(13) VALUE '2N200FH   360'.
           05 FILLER            PIC X(13) VALUE '2N200FEH  360'.
           05 FILLER            PIC X(13) VALUE '2N200FIH  360'.
           05 FILLER            PIC X(13) VALUE '2N200FIRH 360'.
           05 FILLER            PIC X(13) VALUE '2N200FIREH360'.
           05 FILLER            PIC X(13) VALUE '2N200FI   360'.
           05 FILLER            PIC X(13) VALUE '2N200FIR  360'.
           05 FILLER            PIC X(13) VALUE '2N200FIRE 360'.
           05 FILLER            PIC X(13) VALUE '2F150M    360'.
           05 FILLER            PIC X(13) VALUE '2F150ME   360'.
           05 FILLER            PIC X(13) VALUE '2F200M    360'.
           05 FILLER            PIC X(13) VALUE '2F200ME   360'.
           05 FILLER            PIC X(13) VALUE '2N150M    360'.
           05 FILLER            PIC X(13) VALUE '2N150ME   360'.
           05 FILLER            PIC X(13) VALUE '2N150MH   360'.
           05 FILLER            PIC X(13) VALUE '2N150MEH  360'.
           05 FILLER            PIC X(13) VALUE '2N150MIH  360'.
           05 FILLER            PIC X(13) VALUE '2N150MIRH 360'.
           05 FILLER            PIC X(13) VALUE '2N150MIREH360'.
           05 FILLER            PIC X(13) VALUE '2N150MI   360'.
           05 FILLER            PIC X(13) VALUE '2N150MIR  360'.
           05 FILLER            PIC X(13) VALUE '2N150MIRE 360'.
           05 FILLER            PIC X(13) VALUE '2N200M    360'.
           05 FILLER            PIC X(13) VALUE '2N200ME   360'.
           05 FILLER            PIC X(13) VALUE '2N200MH   360'.
           05 FILLER            PIC X(13) VALUE '2N200MEH  360'.
           05 FILLER            PIC X(13) VALUE '2N200MIH  360'.
           05 FILLER            PIC X(13) VALUE '2N200MIRH 360'.
           05 FILLER            PIC X(13) VALUE '2N200MIREH360'.
           05 FILLER            PIC X(13) VALUE '2N200MI   360'.
           05 FILLER            PIC X(13) VALUE '2N200MIR  360'.
           05 FILLER            PIC X(13) VALUE '2N200MIRE 360'.
           05 FILLER            PIC X(13) VALUE '3FF       360'.
           05 FILLER            PIC X(13) VALUE '3NF       360'.
           05 FILLER            PIC X(13) VALUE '3NFI      360'.
           05 FILLER            PIC X(13) VALUE '3FM       360'.
           05 FILLER            PIC X(13) VALUE '3FMI      360'.
           05 FILLER            PIC X(13) VALUE '3NM       360'.
           05 FILLER            PIC X(13) VALUE '3NMI      360'.
           05 FILLER            PIC X(13) VALUE 'NVAGF     300'.
           05 FILLER            PIC X(13) VALUE 'NVAZF     300'.
           05 FILLER            PIC X(13) VALUE 'NVAGM     300'.
           05 FILLER            PIC X(13) VALUE 'NVAZM     300'.
           05 FILLER            PIC X(13) VALUE 'RFF       300'.
           05 FILLER            PIC X(13) VALUE 'RFFE      300'.
           05 FILLER            PIC X(13) VALUE 'RNF       360'.
           05 FILLER            PIC X(13) VALUE 'RNFE      360'.
           05 FILLER            PIC X(13) VALUE 'RNFH      360'.
           05 FILLER            PIC X(13) VALUE 'RNFEH     360'.
           05 FILLER            PIC X(13) VALUE 'RFM       300'.
           05 FILLER            PIC X(13) VALUE 'RFME      300'.
           05 FILLER            PIC X(13) VALUE 'RNM       360'.
           05 FILLER            PIC X(13) VALUE 'RNME      360'.
           05 FILLER            PIC X(13) VALUE 'RNMH      360'.
           05 FILLER            PIC X(13) VALUE 'RNMEH     360'.
           05 FILLER            PIC X(13) VALUE 'RNMIH     360'.
           05 FILLER            PIC X(13) VALUE 'RNMIRH    360'.
           05 FILLER            PIC X(13) VALUE 'RNMIREH   360'.
           05 FILLER            PIC X(13) VALUE '4LF       442'.
           05 FILLER            PIC X(13) VALUE '4LFD      442'.
           05 FILLER            PIC X(13) VALUE '4LFE      442'.
           05 FILLER            PIC X(13) VALUE '4LFEH     442'.
           05 FILLER            PIC X(13) VALUE '4LFH      442'.
           05 FILLER            PIC X(13) VALUE '4LFHD     442'.
           05 FILLER            PIC X(13) VALUE '4LFW      442'.
           05 FILLER            PIC X(13) VALUE '4LFWD     442'.
           05 FILLER            PIC X(13) VALUE '4LFWE     442'.
           05 FILLER            PIC X(13) VALUE '4LFWEH    442'.
           05 FILLER            PIC X(13) VALUE '4LFWH     442'.
           05 FILLER            PIC X(13) VALUE '4LFWHD    442'.
           05 FILLER            PIC X(13) VALUE '4LM       442'.
           05 FILLER            PIC X(13) VALUE '4LMD      442'.
           05 FILLER            PIC X(13) VALUE '4LME      442'.
           05 FILLER            PIC X(13) VALUE '4LMEH     442'.
           05 FILLER            PIC X(13) VALUE '4LMH      442'.
           05 FILLER            PIC X(13) VALUE '4LMHD     442'.
           05 FILLER            PIC X(13) VALUE '4LMW      442'.
           05 FILLER            PIC X(13) VALUE '4LMWD     442'.
           05 FILLER            PIC X(13) VALUE '4LMWE     442'.
           05 FILLER            PIC X(13) VALUE '4LMWEH    442'.
           05 FILLER            PIC X(13) VALUE '4LMWH     442'.
           05 FILLER            PIC X(13) VALUE '4LMWHD    442'.
           05 FILLER            PIC X(13) VALUE '6L5F      442'.
           05 FILLER            PIC X(13) VALUE '6L5FD     442'.
           05 FILLER            PIC X(13) VALUE '6L5FE     442'.
           05 FILLER            PIC X(13) VALUE '6L5FEH    442'.
           05 FILLER            PIC X(13) VALUE '6L5FH     442'.
           05 FILLER            PIC X(13) VALUE '6L5FHD    442'.
           05 FILLER            PIC X(13) VALUE '6L5FW     442'.
           05 FILLER            PIC X(13) VALUE '6L5FWD    442'.
           05 FILLER            PIC X(13) VALUE '6L5FWE    442'.
           05 FILLER            PIC X(13) VALUE '6L5FWEH   442'.
           05 FILLER            PIC X(13) VALUE '6L5FWH    442'.
           05 FILLER            PIC X(13) VALUE '6L5FWHD   442'.
           05 FILLER            PIC X(13) VALUE '6L5M      442'.
           05 FILLER            PIC X(13) VALUE '6L5MD     442'.
           05 FILLER            PIC X(13) VALUE '6L5ME     442'.
           05 FILLER            PIC X(13) VALUE '6L5MEH    442'.
           05 FILLER            PIC X(13) VALUE '6L5MH     442'.
           05 FILLER            PIC X(13) VALUE '6L5MHD    442'.
           05 FILLER            PIC X(13) VALUE '6L5MW     442'.
           05 FILLER            PIC X(13) VALUE '6L5MWD    442'.
           05 FILLER            PIC X(13) VALUE '6L5MWE    442'.
           05 FILLER            PIC X(13) VALUE '6L5MWEH   442'.
           05 FILLER            PIC X(13) VALUE '6L5MWH    442'.
           05 FILLER            PIC X(13) VALUE '6L5MWHD   442'.
           05 FILLER            PIC X(13) VALUE '6L10F     442'.
           05 FILLER            PIC X(13) VALUE '6L10FD    442'.
           05 FILLER            PIC X(13) VALUE '6L10FE    442'.
           05 FILLER            PIC X(13) VALUE '6L10FEH   442'.
           05 FILLER            PIC X(13) VALUE '6L10FH    442'.
           05 FILLER            PIC X(13) VALUE '6L10FHD   442'.
           05 FILLER            PIC X(13) VALUE '6L10FW    442'.
           05 FILLER            PIC X(13) VALUE '6L10FWD   442'.
           05 FILLER            PIC X(13) VALUE '6L10FWE   442'.
           05 FILLER            PIC X(13) VALUE '6L10FWEH  442'.
           05 FILLER            PIC X(13) VALUE '6L10FWH   442'.
           05 FILLER            PIC X(13) VALUE '6L10FWHD  442'.
           05 FILLER            PIC X(13) VALUE '6L10M     442'.
           05 FILLER            PIC X(13) VALUE '6L10MD    442'.
           05 FILLER            PIC X(13) VALUE '6L10ME    442'.
           05 FILLER            PIC X(13) VALUE '6L10MEH   442'.
           05 FILLER            PIC X(13) VALUE '6L10MH    442'.
           05 FILLER            PIC X(13) VALUE '6L10MHD   442'.
           05 FILLER            PIC X(13) VALUE '6L10MW    442'.
           05 FILLER            PIC X(13) VALUE '6L10MWD   442'.
           05 FILLER            PIC X(13) VALUE '6L10MWE   442'.
           05 FILLER            PIC X(13) VALUE '6L10MWEH  442'.
           05 FILLER            PIC X(13) VALUE '6L10MWH   442'.
           05 FILLER            PIC X(13) VALUE '6L10MWHD  442'.
           05 FILLER            PIC X(13) VALUE '4KF       560'.
           05 FILLER            PIC X(13) VALUE '4KFE      560'.
           05 FILLER            PIC X(13) VALUE '4KFW      560'.
           05 FILLER            PIC X(13) VALUE '4KFWE     560'.
           05 FILLER            PIC X(13) VALUE '4KM       560'.
           05 FILLER            PIC X(13) VALUE '4KME      560'.
           05 FILLER            PIC X(13) VALUE '4KMW      560'.
           05 FILLER            PIC X(13) VALUE '4KMWE     560'.
           05 FILLER            PIC X(13) VALUE '4LF       561'.
           05 FILLER            PIC X(13) VALUE '4LFE      561'.
           05 FILLER            PIC X(13) VALUE '4LFW      561'.
           05 FILLER            PIC X(13) VALUE '4LFWE     561'.
           05 FILLER            PIC X(13) VALUE '4LM       561'.
           05 FILLER            PIC X(13) VALUE '4LME      561'.
           05 FILLER            PIC X(13) VALUE '4LMW      561'.
           05 FILLER            PIC X(13) VALUE '4LMWE     561'.
           05 FILLER            PIC X(13) VALUE '6K5F      560'.
           05 FILLER            PIC X(13) VALUE '6K5FE     560'.
           05 FILLER            PIC X(13) VALUE '6K5FW     560'.
           05 FILLER            PIC X(13) VALUE '6K5FWE    560'.
           05 FILLER            PIC X(13) VALUE '6K5M      560'.
           05 FILLER            PIC X(13) VALUE '6K5ME     560'.
           05 FILLER            PIC X(13) VALUE '6K5MW     560'.
           05 FILLER            PIC X(13) VALUE '6K5MWE    560'.
           05 FILLER            PIC X(13) VALUE '6L5F      561'.
           05 FILLER            PIC X(13) VALUE '6L5FE     561'.
           05 FILLER            PIC X(13) VALUE '6L5FW     561'.
           05 FILLER            PIC X(13) VALUE '6L5FWE    561'.
           05 FILLER            PIC X(13) VALUE '6L5FH     561'.
           05 FILLER            PIC X(13) VALUE '6L5FEH    561'.
           05 FILLER            PIC X(13) VALUE '6L5FC     561'.
           05 FILLER            PIC X(13) VALUE '6L5FEC    561'.
           05 FILLER            PIC X(13) VALUE '6L5M      561'.
           05 FILLER            PIC X(13) VALUE '6L5ME     561'.
           05 FILLER            PIC X(13) VALUE '6L5MW     561'.
           05 FILLER            PIC X(13) VALUE '6L5MWE    561'.
           05 FILLER            PIC X(13) VALUE '6L5MH     561'.
           05 FILLER            PIC X(13) VALUE '6L5MEH    561'.
           05 FILLER            PIC X(13) VALUE '6L5MC     561'.
           05 FILLER            PIC X(13) VALUE '6L5MEC    561'.
           05 FILLER            PIC X(13) VALUE '6K10F     560'.
           05 FILLER            PIC X(13) VALUE '6K10FE    560'.
           05 FILLER            PIC X(13) VALUE '6K10FW    560'.
           05 FILLER            PIC X(13) VALUE '6K10FWE   560'.
           05 FILLER            PIC X(13) VALUE '6K10M     560'.
           05 FILLER            PIC X(13) VALUE '6K10ME    560'.
           05 FILLER            PIC X(13) VALUE '6K10MW    560'.
           05 FILLER            PIC X(13) VALUE '6K10MWE   560'.
           05 FILLER            PIC X(13) VALUE '6L10F     561'.
           05 FILLER            PIC X(13) VALUE '6L10FE    561'.
           05 FILLER            PIC X(13) VALUE '6L10FW    561'.
           05 FILLER            PIC X(13) VALUE '6L10FWE   561'.
           05 FILLER            PIC X(13) VALUE '6L10M     561'.
           05 FILLER            PIC X(13) VALUE '6L10ME    561'.
           05 FILLER            PIC X(13) VALUE '6L10MW    561'.
           05 FILLER            PIC X(13) VALUE '6L10MWE   561'.
           05 FILLER            PIC X(13) VALUE '1A        622'.
           05 FILLER            PIC X(13) VALUE '1AD       622'.
           05 FILLER            PIC X(13) VALUE '1AE       622'.
           05 FILLER            PIC X(13) VALUE '1AEH      622'.
           05 FILLER            PIC X(13) VALUE '1AH       622'.
           05 FILLER            PIC X(13) VALUE '1AHD      622'.
           05 FILLER            PIC X(13) VALUE '1B        623'.
           05 FILLER            PIC X(13) VALUE '1BD       623'.
           05 FILLER            PIC X(13) VALUE '1BE       623'.
           05 FILLER            PIC X(13) VALUE '1BEH      623'.
           05 FILLER            PIC X(13) VALUE '1BH       623'.
           05 FILLER            PIC X(13) VALUE '1BHD      623'.
           05 FILLER            PIC X(13) VALUE '1FLNF     624'.
           05 FILLER            PIC X(13) VALUE '1FLNFD    624'.
           05 FILLER            PIC X(13) VALUE '1FLNFE    624'.
           05 FILLER            PIC X(13) VALUE '1FLNFEH   624'.
           05 FILLER            PIC X(13) VALUE '1FLNFH    624'.
           05 FILLER            PIC X(13) VALUE '1FLNFHD   624'.
           05 FILLER            PIC X(13) VALUE '1FLNFI    624'.
           05 FILLER            PIC X(13) VALUE '1FLNFID   624'.
           05 FILLER            PIC X(13) VALUE '1FLNFIH   624'.
           05 FILLER            PIC X(13) VALUE '1FLNFIHD  624'.
           05 FILLER            PIC X(13) VALUE '1FLNFIR   624'.
           05 FILLER            PIC X(13) VALUE '1FLNFIRD  624'.
           05 FILLER            PIC X(13) VALUE '1FLNFIRE  624'.
           05 FILLER            PIC X(13) VALUE '1FLNFIREH 624'.
           05 FILLER            PIC X(13) VALUE '1FLNFIRH  624'.
           05 FILLER            PIC X(13) VALUE '1FLNFIRHD 624'.
           05 FILLER            PIC X(13) VALUE '1FLNM     624'.
           05 FILLER            PIC X(13) VALUE '1FLNMD    624'.
           05 FILLER            PIC X(13) VALUE '1FLNME    624'.
           05 FILLER            PIC X(13) VALUE '1FLNMEH   624'.
           05 FILLER            PIC X(13) VALUE '1FLNMH    624'.
           05 FILLER            PIC X(13) VALUE '1FLNMHD   624'.
           05 FILLER            PIC X(13) VALUE '1FLNMI    624'.
           05 FILLER            PIC X(13) VALUE '1FLNMID   624'.
           05 FILLER            PIC X(13) VALUE '1FLNMIH   624'.
           05 FILLER            PIC X(13) VALUE '1FLNMIHD  624'.
           05 FILLER            PIC X(13) VALUE '1FLNMIR   624'.
           05 FILLER            PIC X(13) VALUE '1FLNMIRD  624'.
           05 FILLER            PIC X(13) VALUE '1FLNMIRE  624'.
           05 FILLER            PIC X(13) VALUE '1FLNMIREH 624'.
           05 FILLER            PIC X(13) VALUE '1FLNMIRH  624'.
           05 FILLER            PIC X(13) VALUE '1FLNMIRHD 624'.
           05 FILLER            PIC X(13) VALUE '1FLSF     625'.
           05 FILLER            PIC X(13) VALUE '1FLSFD    625'.
           05 FILLER            PIC X(13) VALUE '1FLSFE    625'.
           05 FILLER            PIC X(13) VALUE '1FLSFEH   625'.
           05 FILLER            PIC X(13) VALUE '1FLSFH    625'.
           05 FILLER            PIC X(13) VALUE '1FLSFHD   625'.
           05 FILLER            PIC X(13) VALUE '1FLSFI    625'.
           05 FILLER            PIC X(13) VALUE '1FLSFID   625'.
           05 FILLER            PIC X(13) VALUE '1FLSFIH   625'.
           05 FILLER            PIC X(13) VALUE '1FLSFIHD  625'.
           05 FILLER            PIC X(13) VALUE '1FLSFIR   625'.
           05 FILLER            PIC X(13) VALUE '1FLSFIRD  625'.
           05 FILLER            PIC X(13) VALUE '1FLSFIRE  625'.
           05 FILLER            PIC X(13) VALUE '1FLSFIREH 625'.
           05 FILLER            PIC X(13) VALUE '1FLSFIRH  625'.
           05 FILLER            PIC X(13) VALUE '1FLSFIRHD 625'.
           05 FILLER            PIC X(13) VALUE '1FLSM     625'.
           05 FILLER            PIC X(13) VALUE '1FLSMD    625'.
           05 FILLER            PIC X(13) VALUE '1FLSME    625'.
           05 FILLER            PIC X(13) VALUE '1FLSMEH   625'.
           05 FILLER            PIC X(13) VALUE '1FLSMH    625'.
           05 FILLER            PIC X(13) VALUE '1FLSMHD   625'.
           05 FILLER            PIC X(13) VALUE '1FLSMI    625'.
           05 FILLER            PIC X(13) VALUE '1FLSMID   625'.
           05 FILLER            PIC X(13) VALUE '1FLSMIH   625'.
           05 FILLER            PIC X(13) VALUE '1FLSMIHD  625'.
           05 FILLER            PIC X(13) VALUE '1FLSMIR   625'.
           05 FILLER            PIC X(13) VALUE '1FLSMIRD  625'.
           05 FILLER            PIC X(13) VALUE '1FLSMIRE  625'.
           05 FILLER            PIC X(13) VALUE '1FLSMIREH 625'.
           05 FILLER            PIC X(13) VALUE '1FLSMIRH  625'.
           05 FILLER            PIC X(13) VALUE '1FLSMIRHD 625'.
           05 FILLER            PIC X(13) VALUE '5FLNF     624'.
           05 FILLER            PIC X(13) VALUE '5FLNFD    624'.
           05 FILLER            PIC X(13) VALUE '5FLNFE    624'.
           05 FILLER            PIC X(13) VALUE '5FLNFEH   624'.
           05 FILLER            PIC X(13) VALUE '5FLNFH    624'.
           05 FILLER            PIC X(13) VALUE '5FLNFHD   624'.
           05 FILLER            PIC X(13) VALUE '5FLNFI    624'.
           05 FILLER            PIC X(13) VALUE '5FLNFID   624'.
           05 FILLER            PIC X(13) VALUE '5FLNFIH   624'.
           05 FILLER            PIC X(13) VALUE '5FLNFIHD  624'.
           05 FILLER            PIC X(13) VALUE '5FLNFIR   624'.
           05 FILLER            PIC X(13) VALUE '5FLNFIRD  624'.
           05 FILLER            PIC X(13) VALUE '5FLNFIRE  624'.
           05 FILLER            PIC X(13) VALUE '5FLNFIREH 624'.
           05 FILLER            PIC X(13) VALUE '5FLNFIRH  624'.
           05 FILLER            PIC X(13) VALUE '5FLNFIRHD 624'.
           05 FILLER            PIC X(13) VALUE '5FLNM     624'.
           05 FILLER            PIC X(13) VALUE '5FLNMD    624'.
           05 FILLER            PIC X(13) VALUE '5FLNME    624'.
           05 FILLER            PIC X(13) VALUE '5FLNMEH   624'.
           05 FILLER            PIC X(13) VALUE '5FLNMH    624'.
           05 FILLER            PIC X(13) VALUE '5FLNMHD   624'.
           05 FILLER            PIC X(13) VALUE '5FLNMI    624'.
           05 FILLER            PIC X(13) VALUE '5FLNMID   624'.
           05 FILLER            PIC X(13) VALUE '5FLNMIH   624'.
           05 FILLER            PIC X(13) VALUE '5FLNMIHD  624'.
           05 FILLER            PIC X(13) VALUE '5FLNMIR   624'.
           05 FILLER            PIC X(13) VALUE '5FLNMIRD  624'.
           05 FILLER            PIC X(13) VALUE '5FLNMIRE  624'.
           05 FILLER            PIC X(13) VALUE '5FLNMIREH 624'.
           05 FILLER            PIC X(13) VALUE '5FLNMIRH  624'.
           05 FILLER            PIC X(13) VALUE '5FLNMIRHD 624'.
           05 FILLER            PIC X(13) VALUE '5FLNFM    624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMD   624'.
           05 FILLER            PIC X(13) VALUE '5FLNFME   624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMEH  624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMH   624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMHD  624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMI   624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMID  624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMIH  624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMIHD 624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMIR  624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMIRD 624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMIRE 624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMIREH624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMIRH 624'.
           05 FILLER            PIC X(13) VALUE '5FLNFMIRHD624'.
           05 FILLER            PIC X(13) VALUE '5FLNMF    624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFD   624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFE   624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFEH  624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFH   624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFHD  624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFI   624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFID  624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFIH  624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFIHD 624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFIR  624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFIRD 624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFIRE 624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFIREH624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFIRH 624'.
           05 FILLER            PIC X(13) VALUE '5FLNMFIRHD624'.
           05 FILLER            PIC X(13) VALUE '5FLSF     625'.
           05 FILLER            PIC X(13) VALUE '5FLSFD    625'.
           05 FILLER            PIC X(13) VALUE '5FLSFE    625'.
           05 FILLER            PIC X(13) VALUE '5FLSFEH   625'.
           05 FILLER            PIC X(13) VALUE '5FLSFH    625'.
           05 FILLER            PIC X(13) VALUE '5FLSFHD   625'.
           05 FILLER            PIC X(13) VALUE '5FLSFI    625'.
           05 FILLER            PIC X(13) VALUE '5FLSFID   625'.
           05 FILLER            PIC X(13) VALUE '5FLSFIH   625'.
           05 FILLER            PIC X(13) VALUE '5FLSFIHD  625'.
           05 FILLER            PIC X(13) VALUE '5FLSFIR   625'.
           05 FILLER            PIC X(13) VALUE '5FLSFIRD  625'.
           05 FILLER            PIC X(13) VALUE '5FLSFIRE  625'.
           05 FILLER            PIC X(13) VALUE '5FLSFIREH 625'.
           05 FILLER            PIC X(13) VALUE '5FLSFIRH  625'.
           05 FILLER            PIC X(13) VALUE '5FLSFIRHD 625'.
           05 FILLER            PIC X(13) VALUE '5FLSM     625'.
           05 FILLER            PIC X(13) VALUE '5FLSMD    625'.
           05 FILLER            PIC X(13) VALUE '5FLSME    625'.
           05 FILLER            PIC X(13) VALUE '5FLSMEH   625'.
           05 FILLER            PIC X(13) VALUE '5FLSMH    625'.
           05 FILLER            PIC X(13) VALUE '5FLSMHD   625'.
           05 FILLER            PIC X(13) VALUE '5FLSMI    625'.
           05 FILLER            PIC X(13) VALUE '5FLSMID   625'.
           05 FILLER            PIC X(13) VALUE '5FLSMIH   625'.
           05 FILLER            PIC X(13) VALUE '5FLSMIHD  625'.
           05 FILLER            PIC X(13) VALUE '5FLSMIR   625'.
           05 FILLER            PIC X(13) VALUE '5FLSMIRD  625'.
           05 FILLER            PIC X(13) VALUE '5FLSMIRE  625'.
           05 FILLER            PIC X(13) VALUE '5FLSMIREH 625'.
           05 FILLER            PIC X(13) VALUE '5FLSMIRH  625'.
           05 FILLER            PIC X(13) VALUE '5FLSMIRHD 625'.
           05 FILLER            PIC X(13) VALUE '5FLSFM    625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMD   625'.
           05 FILLER            PIC X(13) VALUE '5FLSFME   625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMEH  625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMH   625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMHD  625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMI   625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMID  625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMIH  625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMIHD 625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMIR  625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMIRD 625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMIRE 625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMIREH625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMIRH 625'.
           05 FILLER            PIC X(13) VALUE '5FLSFMIRHD625'.
           05 FILLER            PIC X(13) VALUE '5FLSMF    625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFD   625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFE   625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFEH  625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFH   625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFHD  625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFI   625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFID  625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFIH  625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFIHD 625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFIR  625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFIRD 625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFIRE 625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFIREH625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFIRH 625'.
           05 FILLER            PIC X(13) VALUE '5FLSMFIRHD625'.
           05 FILLER            PIC X(13) VALUE '1N10F     201'.
           05 FILLER            PIC X(13) VALUE '1N10FD    201'.
           05 FILLER            PIC X(13) VALUE '1N10FH    201'.
           05 FILLER            PIC X(13) VALUE '1N10FHD   201'.
           05 FILLER            PIC X(13) VALUE '1N10FI    201'.
           05 FILLER            PIC X(13) VALUE '1N10FID   201'.
           05 FILLER            PIC X(13) VALUE '1N10FIH   201'.
           05 FILLER            PIC X(13) VALUE '1N10FIHD  201'.
           05 FILLER            PIC X(13) VALUE '1N10FIR   201'.
           05 FILLER            PIC X(13) VALUE '1N10FIRD  201'.
           05 FILLER            PIC X(13) VALUE '1N10FIRH  201'.
           05 FILLER            PIC X(13) VALUE '1N10FIRHD 201'.
           05 FILLER            PIC X(13) VALUE '1N10M     201'.
           05 FILLER            PIC X(13) VALUE '1N10MD    201'.
           05 FILLER            PIC X(13) VALUE '1N10MH    201'.
           05 FILLER            PIC X(13) VALUE '1N10MHD   201'.
           05 FILLER            PIC X(13) VALUE '1N10MI    201'.
           05 FILLER            PIC X(13) VALUE '1N10MID   201'.
           05 FILLER            PIC X(13) VALUE '1N10MIH   201'.
           05 FILLER            PIC X(13) VALUE '1N10MIHD  201'.
           05 FILLER            PIC X(13) VALUE '1N10MIR   201'.
           05 FILLER            PIC X(13) VALUE '1N10MIRD  201'.
           05 FILLER            PIC X(13) VALUE '1N10MIRH  201'.
           05 FILLER            PIC X(13) VALUE '1N10MIRHD 201'.
           05 FILLER            PIC X(13) VALUE '2N400F    202'.
           05 FILLER            PIC X(13) VALUE '2N400FD   202'.
           05 FILLER            PIC X(13) VALUE '2N400FE   202'.
           05 FILLER            PIC X(13) VALUE '2N400FEH  202'.
           05 FILLER            PIC X(13) VALUE '2N400FH   202'.
           05 FILLER            PIC X(13) VALUE '2N400FHD  202'.
           05 FILLER            PIC X(13) VALUE '2N400FI   202'.
           05 FILLER            PIC X(13) VALUE '2N400FID  202'.
           05 FILLER            PIC X(13) VALUE '2N400FIH  202'.
           05 FILLER            PIC X(13) VALUE '2N400FIHD 202'.
           05 FILLER            PIC X(13) VALUE '2N400FIR  202'.
           05 FILLER            PIC X(13) VALUE '2N400FIRD 202'.
           05 FILLER            PIC X(13) VALUE '2N400FIRE 202'.
           05 FILLER            PIC X(13) VALUE '2N400FIREH202'.
           05 FILLER            PIC X(13) VALUE '2N400FIRH 202'.
           05 FILLER            PIC X(13) VALUE '2N400FIRHD202'.
           05 FILLER            PIC X(13) VALUE '2N400M    202'.
           05 FILLER            PIC X(13) VALUE '2N400MD   202'.
           05 FILLER            PIC X(13) VALUE '2N400ME   202'.
           05 FILLER            PIC X(13) VALUE '2N400MEH  202'.
           05 FILLER            PIC X(13) VALUE '2N400MH   202'.
           05 FILLER            PIC X(13) VALUE '2N400MHD  202'.
           05 FILLER            PIC X(13) VALUE '2N400MI   202'.
           05 FILLER            PIC X(13) VALUE '2N400MID  202'.
           05 FILLER            PIC X(13) VALUE '2N400MIH  202'.
           05 FILLER            PIC X(13) VALUE '2N400MIHD 202'.
           05 FILLER            PIC X(13) VALUE '2N400MIR  202'.
           05 FILLER            PIC X(13) VALUE '2N400MIRD 202'.
           05 FILLER            PIC X(13) VALUE '2N400MIRE 202'.
           05 FILLER            PIC X(13) VALUE '2N400MIREH202'.
           05 FILLER            PIC X(13) VALUE '2N400MIRH 202'.
           05 FILLER            PIC X(13) VALUE '2N400MIRHD202'.
           05 FILLER            PIC X(13) VALUE '3N10F     203'.
           05 FILLER            PIC X(13) VALUE '3N10FD    203'.
           05 FILLER            PIC X(13) VALUE '3N10FH    203'.
           05 FILLER            PIC X(13) VALUE '3N10FHD   203'.
           05 FILLER            PIC X(13) VALUE '3N10FI    203'.
           05 FILLER            PIC X(13) VALUE '3N10FID   203'.
           05 FILLER            PIC X(13) VALUE '3N10FIH   203'.
           05 FILLER            PIC X(13) VALUE '3N10FIHD  203'.
           05 FILLER            PIC X(13) VALUE '3N10FIR   203'.
           05 FILLER            PIC X(13) VALUE '3N10FIRD  203'.
           05 FILLER            PIC X(13) VALUE '3N10FIRH  203'.
           05 FILLER            PIC X(13) VALUE '3N10FIRHD 203'.
           05 FILLER            PIC X(13) VALUE '3N10M     203'.
           05 FILLER            PIC X(13) VALUE '3N10MD    203'.
           05 FILLER            PIC X(13) VALUE '3N10MH    203'.
           05 FILLER            PIC X(13) VALUE '3N10MHD   203'.
           05 FILLER            PIC X(13) VALUE '3N10MI    203'.
           05 FILLER            PIC X(13) VALUE '3N10MID   203'.
           05 FILLER            PIC X(13) VALUE '3N10MIH   203'.
           05 FILLER            PIC X(13) VALUE '3N10MIHD  203'.
           05 FILLER            PIC X(13) VALUE '3N10MIR   203'.
           05 FILLER            PIC X(13) VALUE '3N10MIRD  203'.
           05 FILLER            PIC X(13) VALUE '3N10MIRH  203'.
           05 FILLER            PIC X(13) VALUE '3N10MIRHD 203'.
           05 FILLER            PIC X(13) VALUE '8N10F     208'.
           05 FILLER            PIC X(13) VALUE '8N10FD    208'.
           05 FILLER            PIC X(13) VALUE '8N10FH    208'.
           05 FILLER            PIC X(13) VALUE '8N10FHD   208'.
           05 FILLER            PIC X(13) VALUE '8N10FI    208'.
           05 FILLER            PIC X(13) VALUE '8N10FID   208'.
           05 FILLER            PIC X(13) VALUE '8N10FIH   208'.
           05 FILLER            PIC X(13) VALUE '8N10FIHD  208'.
           05 FILLER            PIC X(13) VALUE '8N10FIR   208'.
           05 FILLER            PIC X(13) VALUE '8N10FIRD  208'.
           05 FILLER            PIC X(13) VALUE '8N10FIRH  208'.
           05 FILLER            PIC X(13) VALUE '8N10FIRHD 208'.
           05 FILLER            PIC X(13) VALUE '8N10M     208'.
           05 FILLER            PIC X(13) VALUE '8N10MD    208'.
           05 FILLER            PIC X(13) VALUE '8N10MH    208'.
           05 FILLER            PIC X(13) VALUE '8N10MHD   208'.
           05 FILLER            PIC X(13) VALUE '8N10MI    208'.
           05 FILLER            PIC X(13) VALUE '8N10MID   208'.
           05 FILLER            PIC X(13) VALUE '8N10MIH   208'.
           05 FILLER            PIC X(13) VALUE '8N10MIHD  208'.
           05 FILLER            PIC X(13) VALUE '8N10MIR   208'.
           05 FILLER            PIC X(13) VALUE '8N10MIRD  208'.
           05 FILLER            PIC X(13) VALUE '8N10MIRH  208'.
           05 FILLER            PIC X(13) VALUE '8N10MIRHD 208'.
           05 FILLER            PIC X(13) VALUE '9N100F    209'.
           05 FILLER            PIC X(13) VALUE '9N100FD   209'.
           05 FILLER            PIC X(13) VALUE '9N100FH   209'.
           05 FILLER            PIC X(13) VALUE '9N100FHD  209'.
           05 FILLER            PIC X(13) VALUE '9N100FI   209'.
           05 FILLER            PIC X(13) VALUE '9N100FID  209'.
           05 FILLER            PIC X(13) VALUE '9N100FIH  209'.
           05 FILLER            PIC X(13) VALUE '9N100FIHD 209'.
           05 FILLER            PIC X(13) VALUE '9N100FIR  209'.
           05 FILLER            PIC X(13) VALUE '9N100FIRD 209'.
           05 FILLER            PIC X(13) VALUE '9N100FIRH 209'.
           05 FILLER            PIC X(13) VALUE '9N100FIRHD209'.
           05 FILLER            PIC X(13) VALUE '9N100M    209'.
           05 FILLER            PIC X(13) VALUE '9N100MD   209'.
           05 FILLER            PIC X(13) VALUE '9N100MH   209'.
           05 FILLER            PIC X(13) VALUE '9N100MHD  209'.
           05 FILLER            PIC X(13) VALUE '9N100MI   209'.
           05 FILLER            PIC X(13) VALUE '9N100MID  209'.
           05 FILLER            PIC X(13) VALUE '9N100MIH  209'.
           05 FILLER            PIC X(13) VALUE '9N100MIHD 209'.
           05 FILLER            PIC X(13) VALUE '9N100MIR  209'.
           05 FILLER            PIC X(13) VALUE '9N100MIRD 209'.
           05 FILLER            PIC X(13) VALUE '9N100MIRH 209'.
           05 FILLER            PIC X(13) VALUE '9N100MIRHD209'.
           05 FILLER            PIC X(13) VALUE '9N100FM   209'.
           05 FILLER            PIC X(13) VALUE '9N100FMD  209'.
           05 FILLER            PIC X(13) VALUE '9N100FMH  209'.
           05 FILLER            PIC X(13) VALUE '9N100FMHD 209'.
           05 FILLER            PIC X(13) VALUE '9N100FMI  209'.
           05 FILLER            PIC X(13) VALUE '9N100FMID 209'.
           05 FILLER            PIC X(13) VALUE '9N100FMIH 209'.
           05 FILLER            PIC X(13) VALUE '9N100FMIHD209'.
           05 FILLER            PIC X(13) VALUE '9N100FMIR 209'.
           05 FILLER            PIC X(13) VALUE '9N100FMIRD209'.
           05 FILLER            PIC X(13) VALUE '9N100FMIRH209'.
           05 FILLER            PIC X(13) VALUE '9N100MF   209'.
           05 FILLER            PIC X(13) VALUE '9N100MFD  209'.
           05 FILLER            PIC X(13) VALUE '9N100MFH  209'.
           05 FILLER            PIC X(13) VALUE '9N100MFHD 209'.
           05 FILLER            PIC X(13) VALUE '9N100MFI  209'.
           05 FILLER            PIC X(13) VALUE '9N100MFID 209'.
           05 FILLER            PIC X(13) VALUE '9N100MFIH 209'.
           05 FILLER            PIC X(13) VALUE '9N100MFIHD209'.
           05 FILLER            PIC X(13) VALUE '9N100MFIR 209'.
           05 FILLER            PIC X(13) VALUE '9N100MFIRD209'.
           05 FILLER            PIC X(13) VALUE '9N100MFIRH209'.
           05 FILLER            PIC X(13) VALUE '9N150F    209'.
           05 FILLER            PIC X(13) VALUE '9N150FD   209'.
           05 FILLER            PIC X(13) VALUE '9N150FH   209'.
           05 FILLER            PIC X(13) VALUE '9N150FHD  209'.
           05 FILLER            PIC X(13) VALUE '9N150FI   209'.
           05 FILLER            PIC X(13) VALUE '9N150FID  209'.
           05 FILLER            PIC X(13) VALUE '9N150FIH  209'.
           05 FILLER            PIC X(13) VALUE '9N150FIHD 209'.
           05 FILLER            PIC X(13) VALUE '9N150FIR  209'.
           05 FILLER            PIC X(13) VALUE '9N150FIRD 209'.
           05 FILLER            PIC X(13) VALUE '9N150FIRH 209'.
           05 FILLER            PIC X(13) VALUE '9N150FIRHD209'.
           05 FILLER            PIC X(13) VALUE '9N150M    209'.
           05 FILLER            PIC X(13) VALUE '9N150MD   209'.
           05 FILLER            PIC X(13) VALUE '9N150MH   209'.
           05 FILLER            PIC X(13) VALUE '9N150MHD  209'.
           05 FILLER            PIC X(13) VALUE '9N150MI   209'.
           05 FILLER            PIC X(13) VALUE '9N150MID  209'.
           05 FILLER            PIC X(13) VALUE '9N150MIH  209'.
           05 FILLER            PIC X(13) VALUE '9N150MIHD 209'.
           05 FILLER            PIC X(13) VALUE '9N150MIR  209'.
           05 FILLER            PIC X(13) VALUE '9N150MIRD 209'.
           05 FILLER            PIC X(13) VALUE '9N150MIRH 209'.
           05 FILLER            PIC X(13) VALUE '9N150MIRHD209'.
           05 FILLER            PIC X(13) VALUE '9N150FM   209'.
           05 FILLER            PIC X(13) VALUE '9N150FMD  209'.
           05 FILLER            PIC X(13) VALUE '9N150FMH  209'.
           05 FILLER            PIC X(13) VALUE '9N150FMHD 209'.
           05 FILLER            PIC X(13) VALUE '9N150FMI  209'.
           05 FILLER            PIC X(13) VALUE '9N150FMID 209'.
           05 FILLER            PIC X(13) VALUE '9N150FMIH 209'.
           05 FILLER            PIC X(13) VALUE '9N150FMIHD209'.
           05 FILLER            PIC X(13) VALUE '9N150FMIR 209'.
           05 FILLER            PIC X(13) VALUE '9N150FMIRD209'.
           05 FILLER            PIC X(13) VALUE '9N150FMIRH209'.
           05 FILLER            PIC X(13) VALUE '9N150MF   209'.
           05 FILLER            PIC X(13) VALUE '9N150MFD  209'.
           05 FILLER            PIC X(13) VALUE '9N150MFH  209'.
           05 FILLER            PIC X(13) VALUE '9N150MFHD 209'.
           05 FILLER            PIC X(13) VALUE '9N150MFI  209'.
           05 FILLER            PIC X(13) VALUE '9N150MFID 209'.
           05 FILLER            PIC X(13) VALUE '9N150MFIH 209'.
           05 FILLER            PIC X(13) VALUE '9N150MFIHD209'.
           05 FILLER            PIC X(13) VALUE '9N150MFIR 209'.
           05 FILLER            PIC X(13) VALUE '9N150MFIRD209'.
           05 FILLER            PIC X(13) VALUE '9N150MFIRH209'.
           05 FILLER            PIC X(13) VALUE '9N200F    209'.
           05 FILLER            PIC X(13) VALUE '9N200FD   209'.
           05 FILLER            PIC X(13) VALUE '9N200FH   209'.
           05 FILLER            PIC X(13) VALUE '9N200FHD  209'.
           05 FILLER            PIC X(13) VALUE '9N200FI   209'.
           05 FILLER            PIC X(13) VALUE '9N200FID  209'.
           05 FILLER            PIC X(13) VALUE '9N200FIH  209'.
           05 FILLER            PIC X(13) VALUE '9N200FIHD 209'.
           05 FILLER            PIC X(13) VALUE '9N200FIR  209'.
           05 FILLER            PIC X(13) VALUE '9N200FIRD 209'.
           05 FILLER            PIC X(13) VALUE '9N200FIRH 209'.
           05 FILLER            PIC X(13) VALUE '9N200FIRHD209'.
           05 FILLER            PIC X(13) VALUE '9N200M    209'.
           05 FILLER            PIC X(13) VALUE '9N200MD   209'.
           05 FILLER            PIC X(13) VALUE '9N200MH   209'.
           05 FILLER            PIC X(13) VALUE '9N200MHD  209'.
           05 FILLER            PIC X(13) VALUE '9N200MI   209'.
           05 FILLER            PIC X(13) VALUE '9N200MID  209'.
           05 FILLER            PIC X(13) VALUE '9N200MIH  209'.
           05 FILLER            PIC X(13) VALUE '9N200MIHD 209'.
           05 FILLER            PIC X(13) VALUE '9N200MIR  209'.
           05 FILLER            PIC X(13) VALUE '9N200MIRD 209'.
           05 FILLER            PIC X(13) VALUE '9N200MIRH 209'.
           05 FILLER            PIC X(13) VALUE '9N200MIRHD209'.
           05 FILLER            PIC X(13) VALUE '9N200FM   209'.
           05 FILLER            PIC X(13) VALUE '9N200FMD  209'.
           05 FILLER            PIC X(13) VALUE '9N200FMH  209'.
           05 FILLER            PIC X(13) VALUE '9N200FMHD 209'.
           05 FILLER            PIC X(13) VALUE '9N200FMI  209'.
           05 FILLER            PIC X(13) VALUE '9N200FMID 209'.
           05 FILLER            PIC X(13) VALUE '9N200FMIH 209'.
           05 FILLER            PIC X(13) VALUE '9N200FMIHD209'.
           05 FILLER            PIC X(13) VALUE '9N200FMIR 209'.
           05 FILLER            PIC X(13) VALUE '9N200FMIRD209'.
           05 FILLER            PIC X(13) VALUE '9N200FMIRH209'.
           05 FILLER            PIC X(13) VALUE '9N200MF   209'.
           05 FILLER            PIC X(13) VALUE '9N200MFD  209'.
           05 FILLER            PIC X(13) VALUE '9N200MFH  209'.
           05 FILLER            PIC X(13) VALUE '9N200MFHD 209'.
           05 FILLER            PIC X(13) VALUE '9N200MFI  209'.
           05 FILLER            PIC X(13) VALUE '9N200MFID 209'.
           05 FILLER            PIC X(13) VALUE '9N200MFIH 209'.
           05 FILLER            PIC X(13) VALUE '9N200MFIHD209'.
           05 FILLER            PIC X(13) VALUE '9N200MFIR 209'.
           05 FILLER            PIC X(13) VALUE '9N200MFIRD209'.
           05 FILLER            PIC X(13) VALUE '9N200MFIRH209'.
           05 FILLER            PIC X(13) VALUE '3NFIR     203'.
           05 FILLER            PIC X(13) VALUE '3NFIRD    203'.
           05 FILLER            PIC X(13) VALUE '3NFIRH    203'.
           05 FILLER            PIC X(13) VALUE '3NFIRHD   203'.
           05 FILLER            PIC X(13) VALUE '3NMIR     203'.
           05 FILLER            PIC X(13) VALUE '3NMIRD    203'.
           05 FILLER            PIC X(13) VALUE '3NMIRH    203'.
           05 FILLER            PIC X(13) VALUE '3NMIRHD   203'.
           05 FILLER            PIC X(13) VALUE '5NFI      205'.
           05 FILLER            PIC X(13) VALUE '5NFID     205'.
           05 FILLER            PIC X(13) VALUE '5NFIH     205'.
           05 FILLER            PIC X(13) VALUE '5NFIHD    205'.
           05 FILLER            PIC X(13) VALUE '5NFIR     205'.
           05 FILLER            PIC X(13) VALUE '5NFIRD    205'.
           05 FILLER            PIC X(13) VALUE '5NFIRH    205'.
           05 FILLER            PIC X(13) VALUE '5NFIRHD   205'.
           05 FILLER            PIC X(13) VALUE '5NMI      205'.
           05 FILLER            PIC X(13) VALUE '5NMID     205'.
           05 FILLER            PIC X(13) VALUE '5NMIH     205'.
           05 FILLER            PIC X(13) VALUE '5NMIHD    205'.
           05 FILLER            PIC X(13) VALUE '5NMIR     205'.
           05 FILLER            PIC X(13) VALUE '5NMIRD    205'.
           05 FILLER            PIC X(13) VALUE '5NMIRH    205'.
           05 FILLER            PIC X(13) VALUE '5NMIRHD   205'.
           05 FILLER            PIC X(13) VALUE '5NFMI     205'.
           05 FILLER            PIC X(13) VALUE '5NFMID    205'.
           05 FILLER            PIC X(13) VALUE '5NFMIH    205'.
           05 FILLER            PIC X(13) VALUE '5NFMIHD   205'.
           05 FILLER            PIC X(13) VALUE '5NMFI     205'.
           05 FILLER            PIC X(13) VALUE '5NMFID    205'.
           05 FILLER            PIC X(13) VALUE '5NMFIH    205'.
           05 FILLER            PIC X(13) VALUE '5NMFIHD   205'.
           05 FILLER            PIC X(13) VALUE '5NFMIR    205'.
           05 FILLER            PIC X(13) VALUE '5NFMIRD   205'.
           05 FILLER            PIC X(13) VALUE '5NFMIRH   205'.
           05 FILLER            PIC X(13) VALUE '5NFMIRHD  205'.
           05 FILLER            PIC X(13) VALUE '5NMFIR    205'.
           05 FILLER            PIC X(13) VALUE '5NMFIRD   205'.
           05 FILLER            PIC X(13) VALUE '5NMFIRH   205'.
           05 FILLER            PIC X(13) VALUE '5NMFIRHD  205'.
           05 FILLER            PIC X(13) VALUE '1FLRF     643'.
           05 FILLER            PIC X(13) VALUE '1FLRFD    643'.
           05 FILLER            PIC X(13) VALUE '1FLRFE    643'.
           05 FILLER            PIC X(13) VALUE '1FLRFEH   643'.
           05 FILLER            PIC X(13) VALUE '1FLRFH    643'.
           05 FILLER            PIC X(13) VALUE '1FLRFHD   643'.
           05 FILLER            PIC X(13) VALUE '1FLRFI    643'.
           05 FILLER            PIC X(13) VALUE '1FLRFID   643'.
           05 FILLER            PIC X(13) VALUE '1FLRFIH   643'.
           05 FILLER            PIC X(13) VALUE '1FLRFIHD  643'.
           05 FILLER            PIC X(13) VALUE '1FLRFIR   643'.
           05 FILLER            PIC X(13) VALUE '1FLRFIRD  643'.
           05 FILLER            PIC X(13) VALUE '1FLRFIRE  643'.
           05 FILLER            PIC X(13) VALUE '1FLRFIREH 643'.
           05 FILLER            PIC X(13) VALUE '1FLRFIRH  643'.
           05 FILLER            PIC X(13) VALUE '1FLRFIRHD 643'.
           05 FILLER            PIC X(13) VALUE '1FLRM     643'.
           05 FILLER            PIC X(13) VALUE '1FLRMD    643'.
           05 FILLER            PIC X(13) VALUE '1FLRME    643'.
           05 FILLER            PIC X(13) VALUE '1FLRMEH   643'.
           05 FILLER            PIC X(13) VALUE '1FLRMH    643'.
           05 FILLER            PIC X(13) VALUE '1FLRMHD   643'.
           05 FILLER            PIC X(13) VALUE '1FLRMI    643'.
           05 FILLER            PIC X(13) VALUE '1FLRMID   643'.
           05 FILLER            PIC X(13) VALUE '1FLRMIH   643'.
           05 FILLER            PIC X(13) VALUE '1FLRMIHD  643'.
           05 FILLER            PIC X(13) VALUE '1FLRMIR   643'.
           05 FILLER            PIC X(13) VALUE '1FLRMIRD  643'.
           05 FILLER            PIC X(13) VALUE '1FLRMIRE  643'.
           05 FILLER            PIC X(13) VALUE '1FLRMIREH 643'.
           05 FILLER            PIC X(13) VALUE '1FLRMIRH  643'.
           05 FILLER            PIC X(13) VALUE '1FLRMIRHD 643'.
190303*>>>>>
           05 FILLER            PIC X(13) VALUE '1FLR60F   649'.
           05 FILLER            PIC X(13) VALUE '1FLR60FD  649'.
           05 FILLER            PIC X(13) VALUE '1FLR60FI  649'.
           05 FILLER            PIC X(13) VALUE '1FLR60FID 649'.
           05 FILLER            PIC X(13) VALUE '1FLR60FIR 649'.
           05 FILLER            PIC X(13) VALUE '1FLR60FIRD649'.
           05 FILLER            PIC X(13) VALUE '1FLR60M   649'.
           05 FILLER            PIC X(13) VALUE '1FLR60MD  649'.
           05 FILLER            PIC X(13) VALUE '1FLR60MI  649'.
           05 FILLER            PIC X(13) VALUE '1FLR60MID 649'.
           05 FILLER            PIC X(13) VALUE '1FLR60MIR 649'.
           05 FILLER            PIC X(13) VALUE '1FLR60MIRD649'.
190303*<<<<<
           05 FILLER            PIC X(13) VALUE '1KF       271'.
           05 FILLER            PIC X(13) VALUE '1KFD      271'.
           05 FILLER            PIC X(13) VALUE '1KFE      271'.
           05 FILLER            PIC X(13) VALUE '1KFEH     271'.
           05 FILLER            PIC X(13) VALUE '1KFH      271'.
           05 FILLER            PIC X(13) VALUE '1KFHD     271'.
           05 FILLER            PIC X(13) VALUE '1KFI      271'.
           05 FILLER            PIC X(13) VALUE '1KFID     271'.
           05 FILLER            PIC X(13) VALUE '1KFIH     271'.
           05 FILLER            PIC X(13) VALUE '1KFIHD    271'.
           05 FILLER            PIC X(13) VALUE '1KFIR     271'.
           05 FILLER            PIC X(13) VALUE '1KFIRD    271'.
           05 FILLER            PIC X(13) VALUE '1KFIRE    271'.
           05 FILLER            PIC X(13) VALUE '1KFIREH   271'.
           05 FILLER            PIC X(13) VALUE '1KFIRH    271'.
           05 FILLER            PIC X(13) VALUE '1KFIRHD   271'.
      *
           05 FILLER            PIC X(13) VALUE '1KM       271'.
           05 FILLER            PIC X(13) VALUE '1KMD      271'.
           05 FILLER            PIC X(13) VALUE '1KME      271'.
           05 FILLER            PIC X(13) VALUE '1KMEH     271'.
           05 FILLER            PIC X(13) VALUE '1KMH      271'.
           05 FILLER            PIC X(13) VALUE '1KMHD     271'.
           05 FILLER            PIC X(13) VALUE '1KMI      271'.
           05 FILLER            PIC X(13) VALUE '1KMID     271'.
           05 FILLER            PIC X(13) VALUE '1KMIH     271'.
           05 FILLER            PIC X(13) VALUE '1KMIHD    271'.
           05 FILLER            PIC X(13) VALUE '1KMIR     271'.
           05 FILLER            PIC X(13) VALUE '1KMIRD    271'.
           05 FILLER            PIC X(13) VALUE '1KMIRE    271'.
           05 FILLER            PIC X(13) VALUE '1KMIREH   271'.
           05 FILLER            PIC X(13) VALUE '1KMIRH    271'.
           05 FILLER            PIC X(13) VALUE '1KMIRHD   271'.
      *
           05 FILLER            PIC X(13) VALUE '1K10F     271'.
           05 FILLER            PIC X(13) VALUE '1K10FD    271'.
           05 FILLER            PIC X(13) VALUE '1K10FH    271'.
           05 FILLER            PIC X(13) VALUE '1K10FHD   271'.
           05 FILLER            PIC X(13) VALUE '1K10FI    271'.
           05 FILLER            PIC X(13) VALUE '1K10FID   271'.
           05 FILLER            PIC X(13) VALUE '1K10FIH   271'.
           05 FILLER            PIC X(13) VALUE '1K10FIHD  271'.
           05 FILLER            PIC X(13) VALUE '1K10FIR   271'.
           05 FILLER            PIC X(13) VALUE '1K10FIRD  271'.
           05 FILLER            PIC X(13) VALUE '1K10FIRH  271'.
           05 FILLER            PIC X(13) VALUE '1K10FIRHD 271'.
      *
           05 FILLER            PIC X(13) VALUE '1K10M     271'.
           05 FILLER            PIC X(13) VALUE '1K10MD    271'.
           05 FILLER            PIC X(13) VALUE '1K10MH    271'.
           05 FILLER            PIC X(13) VALUE '1K10MHD   271'.
           05 FILLER            PIC X(13) VALUE '1K10MI    271'.
           05 FILLER            PIC X(13) VALUE '1K10MID   271'.
           05 FILLER            PIC X(13) VALUE '1K10MIH   271'.
           05 FILLER            PIC X(13) VALUE '1K10MIHD  271'.
           05 FILLER            PIC X(13) VALUE '1K10MIR   271'.
           05 FILLER            PIC X(13) VALUE '1K10MIRD  271'.
           05 FILLER            PIC X(13) VALUE '1K10MIRH  271'.
           05 FILLER            PIC X(13) VALUE '1K10MIRHD 271'.
      *
           05 FILLER            PIC X(13) VALUE '2K150F    272'.
           05 FILLER            PIC X(13) VALUE '2K150FD   272'.
           05 FILLER            PIC X(13) VALUE '2K150FE   272'.
           05 FILLER            PIC X(13) VALUE '2K150FEH  272'.
           05 FILLER            PIC X(13) VALUE '2K150FH   272'.
           05 FILLER            PIC X(13) VALUE '2K150FHD  272'.
           05 FILLER            PIC X(13) VALUE '2K150FI   272'.
           05 FILLER            PIC X(13) VALUE '2K150FID  272'.
           05 FILLER            PIC X(13) VALUE '2K150FIH  272'.
           05 FILLER            PIC X(13) VALUE '2K150FIHD 272'.
           05 FILLER            PIC X(13) VALUE '2K150FIR  272'.
           05 FILLER            PIC X(13) VALUE '2K150FIRD 272'.
           05 FILLER            PIC X(13) VALUE '2K150FIRE 272'.
           05 FILLER            PIC X(13) VALUE '2K150FIREH272'.
           05 FILLER            PIC X(13) VALUE '2K150FIRH 272'.
           05 FILLER            PIC X(13) VALUE '2K150FIRHD272'.
      *
           05 FILLER            PIC X(13) VALUE '2K150M    272'.
           05 FILLER            PIC X(13) VALUE '2K150MD   272'.
           05 FILLER            PIC X(13) VALUE '2K150ME   272'.
           05 FILLER            PIC X(13) VALUE '2K150MEH  272'.
           05 FILLER            PIC X(13) VALUE '2K150MH   272'.
           05 FILLER            PIC X(13) VALUE '2K150MHD  272'.
           05 FILLER            PIC X(13) VALUE '2K150MI   272'.
           05 FILLER            PIC X(13) VALUE '2K150MID  272'.
           05 FILLER            PIC X(13) VALUE '2K150MIH  272'.
           05 FILLER            PIC X(13) VALUE '2K150MIHD 272'.
           05 FILLER            PIC X(13) VALUE '2K150MIR  272'.
           05 FILLER            PIC X(13) VALUE '2K150MIRD 272'.
           05 FILLER            PIC X(13) VALUE '2K150MIRE 272'.
           05 FILLER            PIC X(13) VALUE '2K150MIREH272'.
           05 FILLER            PIC X(13) VALUE '2K150MIRH 272'.
           05 FILLER            PIC X(13) VALUE '2K150MIRHD272'.
      *
           05 FILLER            PIC X(13) VALUE '2K200F    272'.
           05 FILLER            PIC X(13) VALUE '2K200FD   272'.
           05 FILLER            PIC X(13) VALUE '2K200FE   272'.
           05 FILLER            PIC X(13) VALUE '2K200FEH  272'.
           05 FILLER            PIC X(13) VALUE '2K200FH   272'.
           05 FILLER            PIC X(13) VALUE '2K200FHD  272'.
           05 FILLER            PIC X(13) VALUE '2K200FI   272'.
           05 FILLER            PIC X(13) VALUE '2K200FID  272'.
           05 FILLER            PIC X(13) VALUE '2K200FIH  272'.
           05 FILLER            PIC X(13) VALUE '2K200FIHD 272'.
           05 FILLER            PIC X(13) VALUE '2K200FIR  272'.
           05 FILLER            PIC X(13) VALUE '2K200FIRD 272'.
           05 FILLER            PIC X(13) VALUE '2K200FIRE 272'.
           05 FILLER            PIC X(13) VALUE '2K200FIREH272'.
           05 FILLER            PIC X(13) VALUE '2K200FIRH 272'.
           05 FILLER            PIC X(13) VALUE '2K200FIRHD272'.
      *
           05 FILLER            PIC X(13) VALUE '2K200M    272'.
           05 FILLER            PIC X(13) VALUE '2K200MD   272'.
           05 FILLER            PIC X(13) VALUE '2K200ME   272'.
           05 FILLER            PIC X(13) VALUE '2K200MEH  272'.
           05 FILLER            PIC X(13) VALUE '2K200MH   272'.
           05 FILLER            PIC X(13) VALUE '2K200MHD  272'.
           05 FILLER            PIC X(13) VALUE '2K200MI   272'.
           05 FILLER            PIC X(13) VALUE '2K200MID  272'.
           05 FILLER            PIC X(13) VALUE '2K200MIH  272'.
           05 FILLER            PIC X(13) VALUE '2K200MIHD 272'.
           05 FILLER            PIC X(13) VALUE '2K200MIR  272'.
           05 FILLER            PIC X(13) VALUE '2K200MIRD 272'.
           05 FILLER            PIC X(13) VALUE '2K200MIRE 272'.
           05 FILLER            PIC X(13) VALUE '2K200MIREH272'.
           05 FILLER            PIC X(13) VALUE '2K200MIRH 272'.
           05 FILLER            PIC X(13) VALUE '2K200MIRHD272'.
      *
           05 FILLER            PIC X(13) VALUE '2K400F    272'.
           05 FILLER            PIC X(13) VALUE '2K400FD   272'.
           05 FILLER            PIC X(13) VALUE '2K400FE   272'.
           05 FILLER            PIC X(13) VALUE '2K400FEH  272'.
           05 FILLER            PIC X(13) VALUE '2K400FH   272'.
           05 FILLER            PIC X(13) VALUE '2K400FHD  272'.
           05 FILLER            PIC X(13) VALUE '2K400FI   272'.
           05 FILLER            PIC X(13) VALUE '2K400FID  272'.
           05 FILLER            PIC X(13) VALUE '2K400FIH  272'.
           05 FILLER            PIC X(13) VALUE '2K400FIHD 272'.
           05 FILLER            PIC X(13) VALUE '2K400FIR  272'.
           05 FILLER            PIC X(13) VALUE '2K400FIRD 272'.
           05 FILLER            PIC X(13) VALUE '2K400FIRE 272'.
           05 FILLER            PIC X(13) VALUE '2K400FIREH272'.
           05 FILLER            PIC X(13) VALUE '2K400FIRH 272'.
           05 FILLER            PIC X(13) VALUE '2K400FIRHD272'.
      *
           05 FILLER            PIC X(13) VALUE '2K400M    272'.
           05 FILLER            PIC X(13) VALUE '2K400MD   272'.
           05 FILLER            PIC X(13) VALUE '2K400ME   272'.
           05 FILLER            PIC X(13) VALUE '2K400MEH  272'.
           05 FILLER            PIC X(13) VALUE '2K400MH   272'.
           05 FILLER            PIC X(13) VALUE '2K400MHD  272'.
           05 FILLER            PIC X(13) VALUE '2K400MI   272'.
           05 FILLER            PIC X(13) VALUE '2K400MID  272'.
           05 FILLER            PIC X(13) VALUE '2K400MIH  272'.
           05 FILLER            PIC X(13) VALUE '2K400MIHD 272'.
           05 FILLER            PIC X(13) VALUE '2K400MIR  272'.
           05 FILLER            PIC X(13) VALUE '2K400MIRD 272'.
           05 FILLER            PIC X(13) VALUE '2K400MIRE 272'.
           05 FILLER            PIC X(13) VALUE '2K400MIREH272'.
           05 FILLER            PIC X(13) VALUE '2K400MIRH 272'.
           05 FILLER            PIC X(13) VALUE '2K400MIRHD272'.
      *
           05 FILLER            PIC X(13) VALUE '3KF       273'.
           05 FILLER            PIC X(13) VALUE '3KFD      273'.
           05 FILLER            PIC X(13) VALUE '3KFH      273'.
           05 FILLER            PIC X(13) VALUE '3KFHD     273'.
           05 FILLER            PIC X(13) VALUE '3KFI      273'.
           05 FILLER            PIC X(13) VALUE '3KFID     273'.
           05 FILLER            PIC X(13) VALUE '3KFIH     273'.
           05 FILLER            PIC X(13) VALUE '3KFIHD    273'.
           05 FILLER            PIC X(13) VALUE '3KFIR     273'.
           05 FILLER            PIC X(13) VALUE '3KFIRD    273'.
           05 FILLER            PIC X(13) VALUE '3KFIRH    273'.
           05 FILLER            PIC X(13) VALUE '3KFIRHD   273'.
      *
           05 FILLER            PIC X(13) VALUE '3KM       273'.
           05 FILLER            PIC X(13) VALUE '3KMD      273'.
           05 FILLER            PIC X(13) VALUE '3KMH      273'.
           05 FILLER            PIC X(13) VALUE '3KMHD     273'.
           05 FILLER            PIC X(13) VALUE '3KMI      273'.
           05 FILLER            PIC X(13) VALUE '3KMID     273'.
           05 FILLER            PIC X(13) VALUE '3KMIH     273'.
           05 FILLER            PIC X(13) VALUE '3KMIHD    273'.
           05 FILLER            PIC X(13) VALUE '3KMIR     273'.
           05 FILLER            PIC X(13) VALUE '3KMIRD    273'.
           05 FILLER            PIC X(13) VALUE '3KMIRH    273'.
           05 FILLER            PIC X(13) VALUE '3KMIRHD   273'.
      *
           05 FILLER            PIC X(13) VALUE '3K10F     273'.
           05 FILLER            PIC X(13) VALUE '3K10FD    273'.
           05 FILLER            PIC X(13) VALUE '3K10FH    273'.
           05 FILLER            PIC X(13) VALUE '3K10FHD   273'.
           05 FILLER            PIC X(13) VALUE '3K10FI    273'.
           05 FILLER            PIC X(13) VALUE '3K10FID   273'.
           05 FILLER            PIC X(13) VALUE '3K10FIH   273'.
           05 FILLER            PIC X(13) VALUE '3K10FIHD  273'.
           05 FILLER            PIC X(13) VALUE '3K10FIR   273'.
           05 FILLER            PIC X(13) VALUE '3K10FIRD  273'.
           05 FILLER            PIC X(13) VALUE '3K10FIRH  273'.
           05 FILLER            PIC X(13) VALUE '3K10FIRHD 273'.
      *
           05 FILLER            PIC X(13) VALUE '3K10M     273'.
           05 FILLER            PIC X(13) VALUE '3K10MD    273'.
           05 FILLER            PIC X(13) VALUE '3K10MH    273'.
           05 FILLER            PIC X(13) VALUE '3K10MHD   273'.
           05 FILLER            PIC X(13) VALUE '3K10MI    273'.
           05 FILLER            PIC X(13) VALUE '3K10MID   273'.
           05 FILLER            PIC X(13) VALUE '3K10MIH   273'.
           05 FILLER            PIC X(13) VALUE '3K10MIHD  273'.
           05 FILLER            PIC X(13) VALUE '3K10MIR   273'.
           05 FILLER            PIC X(13) VALUE '3K10MIRD  273'.
           05 FILLER            PIC X(13) VALUE '3K10MIRH  273'.
           05 FILLER            PIC X(13) VALUE '3K10MIRHD 273'.
      *
           05 FILLER            PIC X(13) VALUE '4KF       274'.
           05 FILLER            PIC X(13) VALUE '4KFD      274'.
           05 FILLER            PIC X(13) VALUE '4KFH      274'.
           05 FILLER            PIC X(13) VALUE '4KFHD     274'.
           05 FILLER            PIC X(13) VALUE '4KFI      274'.
           05 FILLER            PIC X(13) VALUE '4KFID     274'.
           05 FILLER            PIC X(13) VALUE '4KFIH     274'.
           05 FILLER            PIC X(13) VALUE '4KFIHD    274'.
      *
           05 FILLER            PIC X(13) VALUE '4KM       274'.
           05 FILLER            PIC X(13) VALUE '4KMD      274'.
           05 FILLER            PIC X(13) VALUE '4KMH      274'.
           05 FILLER            PIC X(13) VALUE '4KMHD     274'.
           05 FILLER            PIC X(13) VALUE '4KMI      274'.
           05 FILLER            PIC X(13) VALUE '4KMID     274'.
           05 FILLER            PIC X(13) VALUE '4KMIH     274'.
           05 FILLER            PIC X(13) VALUE '4KMIHD    274'.
      *
           05 FILLER            PIC X(13) VALUE '5KF       275'.
           05 FILLER            PIC X(13) VALUE '5KFD      275'.
           05 FILLER            PIC X(13) VALUE '5KFH      275'.
           05 FILLER            PIC X(13) VALUE '5KFHD     275'.
           05 FILLER            PIC X(13) VALUE '5KFI      275'.
           05 FILLER            PIC X(13) VALUE '5KFID     275'.
           05 FILLER            PIC X(13) VALUE '5KFIH     275'.
           05 FILLER            PIC X(13) VALUE '5KFIHD    275'.
           05 FILLER            PIC X(13) VALUE '5KFIR     275'.
           05 FILLER            PIC X(13) VALUE '5KFIRD    275'.
           05 FILLER            PIC X(13) VALUE '5KFIRH    275'.
           05 FILLER            PIC X(13) VALUE '5KFIRHD   275'.
      *
           05 FILLER            PIC X(13) VALUE '5KM       275'.
           05 FILLER            PIC X(13) VALUE '5KMD      275'.
           05 FILLER            PIC X(13) VALUE '5KMH      275'.
           05 FILLER            PIC X(13) VALUE '5KMHD     275'.
           05 FILLER            PIC X(13) VALUE '5KMI      275'.
           05 FILLER            PIC X(13) VALUE '5KMID     275'.
           05 FILLER            PIC X(13) VALUE '5KMIH     275'.
           05 FILLER            PIC X(13) VALUE '5KMIHD    275'.
           05 FILLER            PIC X(13) VALUE '5KMIR     275'.
           05 FILLER            PIC X(13) VALUE '5KMIRD    275'.
           05 FILLER            PIC X(13) VALUE '5KMIRH    275'.
           05 FILLER            PIC X(13) VALUE '5KMIRHD   275'.
      *
           05 FILLER            PIC X(13) VALUE '5KFM      275'.
           05 FILLER            PIC X(13) VALUE '5KFMD     275'.
           05 FILLER            PIC X(13) VALUE '5KFMH     275'.
           05 FILLER            PIC X(13) VALUE '5KFMHD    275'.
           05 FILLER            PIC X(13) VALUE '5KFMI     275'.
           05 FILLER            PIC X(13) VALUE '5KFMID    275'.
           05 FILLER            PIC X(13) VALUE '5KFMIH    275'.
           05 FILLER            PIC X(13) VALUE '5KFMIHD   275'.
           05 FILLER            PIC X(13) VALUE '5KFMIR    275'.
           05 FILLER            PIC X(13) VALUE '5KFMIRD   275'.
           05 FILLER            PIC X(13) VALUE '5KFMIRH   275'.
           05 FILLER            PIC X(13) VALUE '5KFMIRHD  275'.
      *
           05 FILLER            PIC X(13) VALUE '5KMF      275'.
           05 FILLER            PIC X(13) VALUE '5KMFD     275'.
           05 FILLER            PIC X(13) VALUE '5KMFH     275'.
           05 FILLER            PIC X(13) VALUE '5KMFHD    275'.
           05 FILLER            PIC X(13) VALUE '5KMFI     275'.
           05 FILLER            PIC X(13) VALUE '5KMFID    275'.
           05 FILLER            PIC X(13) VALUE '5KMFIH    275'.
           05 FILLER            PIC X(13) VALUE '5KMFIHD   275'.
           05 FILLER            PIC X(13) VALUE '5KMFIR    275'.
           05 FILLER            PIC X(13) VALUE '5KMFIRD   275'.
           05 FILLER            PIC X(13) VALUE '5KMFIRH   275'.
           05 FILLER            PIC X(13) VALUE '5KMFIRHD  275'.
      *
           05 FILLER            PIC X(13) VALUE '8K10F     278'.
           05 FILLER            PIC X(13) VALUE '8K10FD    278'.
           05 FILLER            PIC X(13) VALUE '8K10FH    278'.
           05 FILLER            PIC X(13) VALUE '8K10FHD   278'.
           05 FILLER            PIC X(13) VALUE '8K10FI    278'.
           05 FILLER            PIC X(13) VALUE '8K10FID   278'.
           05 FILLER            PIC X(13) VALUE '8K10FIH   278'.
           05 FILLER            PIC X(13) VALUE '8K10FIHD  278'.
           05 FILLER            PIC X(13) VALUE '8K10FIR   278'.
           05 FILLER            PIC X(13) VALUE '8K10FIRD  278'.
           05 FILLER            PIC X(13) VALUE '8K10FIRH  278'.
           05 FILLER            PIC X(13) VALUE '8K10FIRHD 278'.
      *
           05 FILLER            PIC X(13) VALUE '8K10M     278'.
           05 FILLER            PIC X(13) VALUE '8K10MD    278'.
           05 FILLER            PIC X(13) VALUE '8K10MH    278'.
           05 FILLER            PIC X(13) VALUE '8K10MHD   278'.
           05 FILLER            PIC X(13) VALUE '8K10MI    278'.
           05 FILLER            PIC X(13) VALUE '8K10MID   278'.
           05 FILLER            PIC X(13) VALUE '8K10MIH   278'.
           05 FILLER            PIC X(13) VALUE '8K10MIHD  278'.
           05 FILLER            PIC X(13) VALUE '8K10MIR   278'.
           05 FILLER            PIC X(13) VALUE '8K10MIRD  278'.
           05 FILLER            PIC X(13) VALUE '8K10MIRH  278'.
           05 FILLER            PIC X(13) VALUE '8K10MIRHD 278'.
      *
           05 FILLER            PIC X(13) VALUE '8K100F    278'.
           05 FILLER            PIC X(13) VALUE '8K100FD   278'.
           05 FILLER            PIC X(13) VALUE '8K100FH   278'.
           05 FILLER            PIC X(13) VALUE '8K100FHD  278'.
           05 FILLER            PIC X(13) VALUE '8K100FI   278'.
           05 FILLER            PIC X(13) VALUE '8K100FID  278'.
           05 FILLER            PIC X(13) VALUE '8K100FIH  278'.
           05 FILLER            PIC X(13) VALUE '8K100FIHD 278'.
           05 FILLER            PIC X(13) VALUE '8K100FIR  278'.
           05 FILLER            PIC X(13) VALUE '8K100FIRD 278'.
           05 FILLER            PIC X(13) VALUE '8K100FIRH 278'.
           05 FILLER            PIC X(13) VALUE '8K100FIRHD278'.
      *
           05 FILLER            PIC X(13) VALUE '8K100M    278'.
           05 FILLER            PIC X(13) VALUE '8K100MD   278'.
           05 FILLER            PIC X(13) VALUE '8K100MH   278'.
           05 FILLER            PIC X(13) VALUE '8K100MHD  278'.
           05 FILLER            PIC X(13) VALUE '8K100MI   278'.
           05 FILLER            PIC X(13) VALUE '8K100MID  278'.
           05 FILLER            PIC X(13) VALUE '8K100MIH  278'.
           05 FILLER            PIC X(13) VALUE '8K100MIHD 278'.
           05 FILLER            PIC X(13) VALUE '8K100MIR  278'.
           05 FILLER            PIC X(13) VALUE '8K100MIRD 278'.
           05 FILLER            PIC X(13) VALUE '8K100MIRH 278'.
           05 FILLER            PIC X(13) VALUE '8K100MIRHD278'.
      *
           05 FILLER            PIC X(13) VALUE '8K150F    278'.
           05 FILLER            PIC X(13) VALUE '8K150FD   278'.
           05 FILLER            PIC X(13) VALUE '8K150FH   278'.
           05 FILLER            PIC X(13) VALUE '8K150FHD  278'.
           05 FILLER            PIC X(13) VALUE '8K150FI   278'.
           05 FILLER            PIC X(13) VALUE '8K150FID  278'.
           05 FILLER            PIC X(13) VALUE '8K150FIH  278'.
           05 FILLER            PIC X(13) VALUE '8K150FIHD 278'.
           05 FILLER            PIC X(13) VALUE '8K150FIR  278'.
           05 FILLER            PIC X(13) VALUE '8K150FIRD 278'.
           05 FILLER            PIC X(13) VALUE '8K150FIRH 278'.
           05 FILLER            PIC X(13) VALUE '8K150FIRHD278'.
      *
           05 FILLER            PIC X(13) VALUE '8K150M    278'.
           05 FILLER            PIC X(13) VALUE '8K150MD   278'.
           05 FILLER            PIC X(13) VALUE '8K150MH   278'.
           05 FILLER            PIC X(13) VALUE '8K150MHD  278'.
           05 FILLER            PIC X(13) VALUE '8K150MI   278'.
           05 FILLER            PIC X(13) VALUE '8K150MID  278'.
           05 FILLER            PIC X(13) VALUE '8K150MIH  278'.
           05 FILLER            PIC X(13) VALUE '8K150MIHD 278'.
           05 FILLER            PIC X(13) VALUE '8K150MIR  278'.
           05 FILLER            PIC X(13) VALUE '8K150MIRD 278'.
           05 FILLER            PIC X(13) VALUE '8K150MIRH 278'.
           05 FILLER            PIC X(13) VALUE '8K150MIRHD278'.
      *
           05 FILLER            PIC X(13) VALUE '8K200F    278'.
           05 FILLER            PIC X(13) VALUE '8K200FD   278'.
           05 FILLER            PIC X(13) VALUE '8K200FH   278'.
           05 FILLER            PIC X(13) VALUE '8K200FHD  278'.
           05 FILLER            PIC X(13) VALUE '8K200FI   278'.
           05 FILLER            PIC X(13) VALUE '8K200FID  278'.
           05 FILLER            PIC X(13) VALUE '8K200FIH  278'.
           05 FILLER            PIC X(13) VALUE '8K200FIHD 278'.
           05 FILLER            PIC X(13) VALUE '8K200FIR  278'.
           05 FILLER            PIC X(13) VALUE '8K200FIRD 278'.
           05 FILLER            PIC X(13) VALUE '8K200FIRH 278'.
           05 FILLER            PIC X(13) VALUE '8K200FIRHD278'.
      *
           05 FILLER            PIC X(13) VALUE '8K200M    278'.
           05 FILLER            PIC X(13) VALUE '8K200MD   278'.
           05 FILLER            PIC X(13) VALUE '8K200MH   278'.
           05 FILLER            PIC X(13) VALUE '8K200MHD  278'.
           05 FILLER            PIC X(13) VALUE '8K200MI   278'.
           05 FILLER            PIC X(13) VALUE '8K200MID  278'.
           05 FILLER            PIC X(13) VALUE '8K200MIH  278'.
           05 FILLER            PIC X(13) VALUE '8K200MIHD 278'.
           05 FILLER            PIC X(13) VALUE '8K200MIR  278'.
           05 FILLER            PIC X(13) VALUE '8K200MIRD 278'.
           05 FILLER            PIC X(13) VALUE '8K200MIRH 278'.
           05 FILLER            PIC X(13) VALUE '8K200MIRHD278'.
      *
           05 FILLER            PIC X(13) VALUE '9K100F    279'.
           05 FILLER            PIC X(13) VALUE '9K100FD   279'.
           05 FILLER            PIC X(13) VALUE '9K100FH   279'.
           05 FILLER            PIC X(13) VALUE '9K100FHD  279'.
           05 FILLER            PIC X(13) VALUE '9K100FI   279'.
           05 FILLER            PIC X(13) VALUE '9K100FID  279'.
           05 FILLER            PIC X(13) VALUE '9K100FIH  279'.
           05 FILLER            PIC X(13) VALUE '9K100FIHD 279'.
           05 FILLER            PIC X(13) VALUE '9K100FIR  279'.
           05 FILLER            PIC X(13) VALUE '9K100FIRD 279'.
           05 FILLER            PIC X(13) VALUE '9K100FIRH 279'.
           05 FILLER            PIC X(13) VALUE '9K100FIRHD279'.
      *
           05 FILLER            PIC X(13) VALUE '9K100M    279'.
           05 FILLER            PIC X(13) VALUE '9K100MD   279'.
           05 FILLER            PIC X(13) VALUE '9K100MH   279'.
           05 FILLER            PIC X(13) VALUE '9K100MHD  279'.
           05 FILLER            PIC X(13) VALUE '9K100MI   279'.
           05 FILLER            PIC X(13) VALUE '9K100MID  279'.
           05 FILLER            PIC X(13) VALUE '9K100MIH  279'.
           05 FILLER            PIC X(13) VALUE '9K100MIHD 279'.
           05 FILLER            PIC X(13) VALUE '9K100MIR  279'.
           05 FILLER            PIC X(13) VALUE '9K100MIRD 279'.
           05 FILLER            PIC X(13) VALUE '9K100MIRH 279'.
           05 FILLER            PIC X(13) VALUE '9K100MIRHD279'.
      *
           05 FILLER            PIC X(13) VALUE '9K100FM   279'.
           05 FILLER            PIC X(13) VALUE '9K100FMD  279'.
           05 FILLER            PIC X(13) VALUE '9K100FMH  279'.
           05 FILLER            PIC X(13) VALUE '9K100FMHD 279'.
           05 FILLER            PIC X(13) VALUE '9K100FMI  279'.
           05 FILLER            PIC X(13) VALUE '9K100FMID 279'.
           05 FILLER            PIC X(13) VALUE '9K100FMIH 279'.
           05 FILLER            PIC X(13) VALUE '9K100FMIHD279'.
           05 FILLER            PIC X(13) VALUE '9K100FMIR 279'.
           05 FILLER            PIC X(13) VALUE '9K100FMIRD279'.
           05 FILLER            PIC X(13) VALUE '9K100FMIRH279'.
      *
           05 FILLER            PIC X(13) VALUE '9K100MF   279'.
           05 FILLER            PIC X(13) VALUE '9K100MFD  279'.
           05 FILLER            PIC X(13) VALUE '9K100MFH  279'.
           05 FILLER            PIC X(13) VALUE '9K100MFHD 279'.
           05 FILLER            PIC X(13) VALUE '9K100MFI  279'.
           05 FILLER            PIC X(13) VALUE '9K100MFID 279'.
           05 FILLER            PIC X(13) VALUE '9K100MFIH 279'.
           05 FILLER            PIC X(13) VALUE '9K100MFIHD279'.
           05 FILLER            PIC X(13) VALUE '9K100MFIR 279'.
           05 FILLER            PIC X(13) VALUE '9K100MFIRD279'.
           05 FILLER            PIC X(13) VALUE '9K100MFIRH279'.
      *
           05 FILLER            PIC X(13) VALUE '9K150F    279'.
           05 FILLER            PIC X(13) VALUE '9K150FD   279'.
           05 FILLER            PIC X(13) VALUE '9K150FH   279'.
           05 FILLER            PIC X(13) VALUE '9K150FHD  279'.
           05 FILLER            PIC X(13) VALUE '9K150FI   279'.
           05 FILLER            PIC X(13) VALUE '9K150FID  279'.
           05 FILLER            PIC X(13) VALUE '9K150FIH  279'.
           05 FILLER            PIC X(13) VALUE '9K150FIHD 279'.
           05 FILLER            PIC X(13) VALUE '9K150FIR  279'.
           05 FILLER            PIC X(13) VALUE '9K150FIRD 279'.
           05 FILLER            PIC X(13) VALUE '9K150FIRH 279'.
           05 FILLER            PIC X(13) VALUE '9K150FIRHD279'.
      *
           05 FILLER            PIC X(13) VALUE '9K150M    279'.
           05 FILLER            PIC X(13) VALUE '9K150MD   279'.
           05 FILLER            PIC X(13) VALUE '9K150MH   279'.
           05 FILLER            PIC X(13) VALUE '9K150MHD  279'.
           05 FILLER            PIC X(13) VALUE '9K150MI   279'.
           05 FILLER            PIC X(13) VALUE '9K150MID  279'.
           05 FILLER            PIC X(13) VALUE '9K150MIH  279'.
           05 FILLER            PIC X(13) VALUE '9K150MIHD 279'.
           05 FILLER            PIC X(13) VALUE '9K150MIR  279'.
           05 FILLER            PIC X(13) VALUE '9K150MIRD 279'.
           05 FILLER            PIC X(13) VALUE '9K150MIRH 279'.
           05 FILLER            PIC X(13) VALUE '9K150MIRHD279'.
      *
           05 FILLER            PIC X(13) VALUE '9K150FM   279'.
           05 FILLER            PIC X(13) VALUE '9K150FMD  279'.
           05 FILLER            PIC X(13) VALUE '9K150FMH  279'.
           05 FILLER            PIC X(13) VALUE '9K150FMHD 279'.
           05 FILLER            PIC X(13) VALUE '9K150FMI  279'.
           05 FILLER            PIC X(13) VALUE '9K150FMID 279'.
           05 FILLER            PIC X(13) VALUE '9K150FMIH 279'.
           05 FILLER            PIC X(13) VALUE '9K150FMIHD279'.
           05 FILLER            PIC X(13) VALUE '9K150FMIR 279'.
           05 FILLER            PIC X(13) VALUE '9K150FMIRD279'.
           05 FILLER            PIC X(13) VALUE '9K150FMIRH279'.
      *
           05 FILLER            PIC X(13) VALUE '9K150MF   279'.
           05 FILLER            PIC X(13) VALUE '9K150MFD  279'.
           05 FILLER            PIC X(13) VALUE '9K150MFH  279'.
           05 FILLER            PIC X(13) VALUE '9K150MFHD 279'.
           05 FILLER            PIC X(13) VALUE '9K150MFI  279'.
           05 FILLER            PIC X(13) VALUE '9K150MFID 279'.
           05 FILLER            PIC X(13) VALUE '9K150MFIH 279'.
           05 FILLER            PIC X(13) VALUE '9K150MFIHD279'.
           05 FILLER            PIC X(13) VALUE '9K150MFIR 279'.
           05 FILLER            PIC X(13) VALUE '9K150MFIRD279'.
           05 FILLER            PIC X(13) VALUE '9K150MFIRH279'.
      *
           05 FILLER            PIC X(13) VALUE '9K200F    279'.
           05 FILLER            PIC X(13) VALUE '9K200FD   279'.
           05 FILLER            PIC X(13) VALUE '9K200FH   279'.
           05 FILLER            PIC X(13) VALUE '9K200FHD  279'.
           05 FILLER            PIC X(13) VALUE '9K200FI   279'.
           05 FILLER            PIC X(13) VALUE '9K200FID  279'.
           05 FILLER            PIC X(13) VALUE '9K200FIH  279'.
           05 FILLER            PIC X(13) VALUE '9K200FIHD 279'.
           05 FILLER            PIC X(13) VALUE '9K200FIR  279'.
           05 FILLER            PIC X(13) VALUE '9K200FIRD 279'.
           05 FILLER            PIC X(13) VALUE '9K200FIRH 279'.
           05 FILLER            PIC X(13) VALUE '9K200FIRHD279'.
      *
           05 FILLER            PIC X(13) VALUE '9K200M    279'.
           05 FILLER            PIC X(13) VALUE '9K200MD   279'.
           05 FILLER            PIC X(13) VALUE '9K200MH   279'.
           05 FILLER            PIC X(13) VALUE '9K200MHD  279'.
           05 FILLER            PIC X(13) VALUE '9K200MI   279'.
           05 FILLER            PIC X(13) VALUE '9K200MID  279'.
           05 FILLER            PIC X(13) VALUE '9K200MIH  279'.
           05 FILLER            PIC X(13) VALUE '9K200MIHD 279'.
           05 FILLER            PIC X(13) VALUE '9K200MIR  279'.
           05 FILLER            PIC X(13) VALUE '9K200MIRD 279'.
           05 FILLER            PIC X(13) VALUE '9K200MIRH 279'.
           05 FILLER            PIC X(13) VALUE '9K200MIRHD279'.
      *
           05 FILLER            PIC X(13) VALUE '9K200FM   279'.
           05 FILLER            PIC X(13) VALUE '9K200FMD  279'.
           05 FILLER            PIC X(13) VALUE '9K200FMH  279'.
           05 FILLER            PIC X(13) VALUE '9K200FMHD 279'.
           05 FILLER            PIC X(13) VALUE '9K200FMI  279'.
           05 FILLER            PIC X(13) VALUE '9K200FMID 279'.
           05 FILLER            PIC X(13) VALUE '9K200FMIH 279'.
           05 FILLER            PIC X(13) VALUE '9K200FMIHD279'.
           05 FILLER            PIC X(13) VALUE '9K200FMIR 279'.
           05 FILLER            PIC X(13) VALUE '9K200FMIRD279'.
           05 FILLER            PIC X(13) VALUE '9K200FMIRH279'.
      *
           05 FILLER            PIC X(13) VALUE '9K200MF   279'.
           05 FILLER            PIC X(13) VALUE '9K200MFD  279'.
           05 FILLER            PIC X(13) VALUE '9K200MFH  279'.
           05 FILLER            PIC X(13) VALUE '9K200MFHD 279'.
           05 FILLER            PIC X(13) VALUE '9K200MFI  279'.
           05 FILLER            PIC X(13) VALUE '9K200MFID 279'.
           05 FILLER            PIC X(13) VALUE '9K200MFIH 279'.
           05 FILLER            PIC X(13) VALUE '9K200MFIHD279'.
           05 FILLER            PIC X(13) VALUE '9K200MFIR 279'.
           05 FILLER            PIC X(13) VALUE '9K200MFIRD279'.
           05 FILLER            PIC X(13) VALUE '9K200MFIRH279'.
      *
           05 FILLER            PIC X(13) VALUE '1KF       360'.
           05 FILLER            PIC X(13) VALUE '1KFE      360'.
           05 FILLER            PIC X(13) VALUE '1KFI      360'.
           05 FILLER            PIC X(13) VALUE '1KFIR     360'.
           05 FILLER            PIC X(13) VALUE '1KFIRE    360'.
           05 FILLER            PIC X(13) VALUE '1KFH      360'.
           05 FILLER            PIC X(13) VALUE '1KFEH     360'.
           05 FILLER            PIC X(13) VALUE '1KFIH     360'.
           05 FILLER            PIC X(13) VALUE '1KFIRH    360'.
           05 FILLER            PIC X(13) VALUE '1KFIREH   360'.
           05 FILLER            PIC X(13) VALUE '1KM       360'.
           05 FILLER            PIC X(13) VALUE '1KME      360'.
           05 FILLER            PIC X(13) VALUE '1KMI      360'.
           05 FILLER            PIC X(13) VALUE '1KMIR     360'.
           05 FILLER            PIC X(13) VALUE '1KMIRE    360'.
           05 FILLER            PIC X(13) VALUE '1KMH      360'.
           05 FILLER            PIC X(13) VALUE '1KMEH     360'.
           05 FILLER            PIC X(13) VALUE '1KMIH     360'.
           05 FILLER            PIC X(13) VALUE '1KMIRH    360'.
           05 FILLER            PIC X(13) VALUE '1KMIREH   360'.
           05 FILLER            PIC X(13) VALUE '2K150F    360'.
           05 FILLER            PIC X(13) VALUE '2K150FE   360'.
           05 FILLER            PIC X(13) VALUE '2K150FH   360'.
           05 FILLER            PIC X(13) VALUE '2K150FEH  360'.
           05 FILLER            PIC X(13) VALUE '2K150FIH  360'.
           05 FILLER            PIC X(13) VALUE '2K150FIRH 360'.
           05 FILLER            PIC X(13) VALUE '2K150FIREH360'.
           05 FILLER            PIC X(13) VALUE '2K150FI   360'.
           05 FILLER            PIC X(13) VALUE '2K150FIR  360'.
           05 FILLER            PIC X(13) VALUE '2K150FIRE 360'.
           05 FILLER            PIC X(13) VALUE '2K200F    360'.
           05 FILLER            PIC X(13) VALUE '2K200FE   360'.
           05 FILLER            PIC X(13) VALUE '2K200FH   360'.
           05 FILLER            PIC X(13) VALUE '2K200FEH  360'.
           05 FILLER            PIC X(13) VALUE '2K200FIH  360'.
           05 FILLER            PIC X(13) VALUE '2K200FIRH 360'.
           05 FILLER            PIC X(13) VALUE '2K200FIREH360'.
           05 FILLER            PIC X(13) VALUE '2K200FI   360'.
           05 FILLER            PIC X(13) VALUE '2K200FIR  360'.
           05 FILLER            PIC X(13) VALUE '2K200FIRE 360'.
           05 FILLER            PIC X(13) VALUE '2K150M    360'.
           05 FILLER            PIC X(13) VALUE '2K150ME   360'.
           05 FILLER            PIC X(13) VALUE '2K150MH   360'.
           05 FILLER            PIC X(13) VALUE '2K150MEH  360'.
           05 FILLER            PIC X(13) VALUE '2K150MIH  360'.
           05 FILLER            PIC X(13) VALUE '2K150MIRH 360'.
           05 FILLER            PIC X(13) VALUE '2K150MIREH360'.
           05 FILLER            PIC X(13) VALUE '2K150MI   360'.
           05 FILLER            PIC X(13) VALUE '2K150MIR  360'.
           05 FILLER            PIC X(13) VALUE '2K150MIRE 360'.
           05 FILLER            PIC X(13) VALUE '2K200M    360'.
           05 FILLER            PIC X(13) VALUE '2K200ME   360'.
           05 FILLER            PIC X(13) VALUE '2K200MH   360'.
           05 FILLER            PIC X(13) VALUE '2K200MEH  360'.
           05 FILLER            PIC X(13) VALUE '2K200MIH  360'.
           05 FILLER            PIC X(13) VALUE '2K200MIRH 360'.
           05 FILLER            PIC X(13) VALUE '2K200MIREH360'.
           05 FILLER            PIC X(13) VALUE '2K200MI   360'.
           05 FILLER            PIC X(13) VALUE '2K200MIR  360'.
           05 FILLER            PIC X(13) VALUE '2K200MIRE 360'.

011295     05 FILLER            PIC X(13) VALUE '1FLF      626'.
011295     05 FILLER            PIC X(13) VALUE '1FLFD     626'.
011295     05 FILLER            PIC X(13) VALUE '1FLFH     626'.
011295     05 FILLER            PIC X(13) VALUE '1FLFHD    626'.
011295     05 FILLER            PIC X(13) VALUE '1FLFI     626'.
011295     05 FILLER            PIC X(13) VALUE '1FLFID    626'.
011295     05 FILLER            PIC X(13) VALUE '1FLFIH    626'.
011295     05 FILLER            PIC X(13) VALUE '1FLFIHD   626'.
011295     05 FILLER            PIC X(13) VALUE '1FLFIR    626'.
011295     05 FILLER            PIC X(13) VALUE '1FLFIRD   626'.
011295     05 FILLER            PIC X(13) VALUE '1FLFIRH   626'.
011295     05 FILLER            PIC X(13) VALUE '1FLFIRHD  626'.

011295     05 FILLER            PIC X(13) VALUE '1FLM      626'.
011295     05 FILLER            PIC X(13) VALUE '1FLMD     626'.
011295     05 FILLER            PIC X(13) VALUE '1FLMH     626'.
011295     05 FILLER            PIC X(13) VALUE '1FLMHD    626'.
011295     05 FILLER            PIC X(13) VALUE '1FLMI     626'.
011295     05 FILLER            PIC X(13) VALUE '1FLMID    626'.
011295     05 FILLER            PIC X(13) VALUE '1FLMIH    626'.
011295     05 FILLER            PIC X(13) VALUE '1FLMIHD   626'.
011295     05 FILLER            PIC X(13) VALUE '1FLMIR    626'.
011295     05 FILLER            PIC X(13) VALUE '1FLMIRD   626'.
011295     05 FILLER            PIC X(13) VALUE '1FLMIRH   626'.
011295     05 FILLER            PIC X(13) VALUE '1FLMIRHD  626'.

011295     05 FILLER            PIC X(13) VALUE '1FL50F    626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50FD   626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50FH   626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50FHD  626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50FI   626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50FID  626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50FIH  626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50FIHD 626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50FIR  626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50FIRD 626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50FIRH 626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50FIRHD626'.

011295     05 FILLER            PIC X(13) VALUE '1FL50M    626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50MD   626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50MH   626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50MHD  626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50MI   626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50MID  626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50MIH  626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50MIHD 626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50MIR  626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50MIRD 626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50MIRH 626'.
011295     05 FILLER            PIC X(13) VALUE '1FL50MIRHD626'.

250396     05 FILLER            PIC X(13) VALUE '1FL60F    626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60FD   626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60FH   626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60FHD  626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60FI   626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60FID  626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60FIH  626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60FIHD 626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60FIR  626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60FIRD 626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60FIRH 626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60FIRHD626'.

250396     05 FILLER            PIC X(13) VALUE '1FL60M    626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60MD   626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60MH   626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60MHD  626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60MI   626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60MID  626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60MIH  626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60MIHD 626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60MIR  626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60MIRD 626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60MIRH 626'.
250396     05 FILLER            PIC X(13) VALUE '1FL60MIRHD626'.

011295     05 FILLER            PIC X(13) VALUE '5FLF      627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFD     627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFH     627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFHD    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFI     627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFID    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFIH    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFIHD   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFIR    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFIRD   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFIRH   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFIRHD  627'.

011295     05 FILLER            PIC X(13) VALUE '5FLM      627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMD     627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMH     627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMHD    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMI     627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMID    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMIH    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMIHD   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMIR    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMIRD   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMIRH   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMIRHD  627'.

011295     05 FILLER            PIC X(13) VALUE '5FLFM     627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFMD    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFMH    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFMHD   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFMI    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFMID   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFMIH   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFMIHD  627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFMIR   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFMIRD  627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFMIRH  627'.
011295     05 FILLER            PIC X(13) VALUE '5FLFMIRHD 627'.

011295     05 FILLER            PIC X(13) VALUE '5FLMF     627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMFD    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMFH    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMFHD   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMFI    627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMFID   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMFIH   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMFIHD  627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMFIR   627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMFIRD  627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMFIRH  627'.
011295     05 FILLER            PIC X(13) VALUE '5FLMFIRHD 627'.

011295     05 FILLER            PIC X(13) VALUE 'SRF       447'.
011295     05 FILLER            PIC X(13) VALUE 'SRFH      447'.
011295     05 FILLER            PIC X(13) VALUE 'SRM       447'.
011295     05 FILLER            PIC X(13) VALUE 'SRMH      447'.
011295     05 FILLER            PIC X(13) VALUE 'SRPF      447'.
011295     05 FILLER            PIC X(13) VALUE 'SRPFH     447'.
011295     05 FILLER            PIC X(13) VALUE 'SRPM      447'.
011295     05 FILLER            PIC X(13) VALUE 'SRPMH     447'.
011295     05 FILLER            PIC X(13) VALUE 'SRPFM     447'.
011295     05 FILLER            PIC X(13) VALUE 'SRPFMH    447'.
011295     05 FILLER            PIC X(13) VALUE 'SRPMF     447'.
011295     05 FILLER            PIC X(13) VALUE 'SRPMFH    447'.

011295     05 FILLER            PIC X(13) VALUE 'ARF       446'.
011295     05 FILLER            PIC X(13) VALUE 'ARFD      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARFE      445'.
011295     05 FILLER            PIC X(13) VALUE 'ARFEH     445'.
011295     05 FILLER            PIC X(13) VALUE 'ARFH      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARFHD     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARFI      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARFID     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARFIH     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARFIHD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARFIR     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARFIRD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARFIRH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARFIRHD   446'.

011295     05 FILLER            PIC X(13) VALUE 'ARM       446'.
011295     05 FILLER            PIC X(13) VALUE 'ARMD      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARME      445'.
011295     05 FILLER            PIC X(13) VALUE 'ARMEH     445'.
011295     05 FILLER            PIC X(13) VALUE 'ARMH      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARMHD     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARMI      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARMID     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARMIH     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARMIHD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARMIR     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARMIRD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARMIRH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARMIRHD   446'.

011295     05 FILLER            PIC X(13) VALUE 'ARBF      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBFD     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBFE     445'.
011295     05 FILLER            PIC X(13) VALUE 'ARBFEH    445'.
011295     05 FILLER            PIC X(13) VALUE 'ARBFH     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBFHD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBFI     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBFID    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBFIH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBFILD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBFIR    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBFIRD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBFIRH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBFIRHD  446'.

011295     05 FILLER            PIC X(13) VALUE 'ARBM      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBMD     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBME     445'.
011295     05 FILLER            PIC X(13) VALUE 'ARBMEH    445'.
011295     05 FILLER            PIC X(13) VALUE 'ARBMH     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBMHD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBMI     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBMID    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBMIH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBMIHD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBMIR    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBMIRD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBMIRH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARBMIRHD  446'.

110100     05 FILLER            PIC X(13) VALUE '8ARBF     444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBFD    444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBFE    444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBFEH   444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBFH    444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBFHD   444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBFI    444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBFID   444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBFIH   444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBFILD  444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBFIR   444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBFIRD  444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBFIRH  444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBFIRHD 444'.

110100     05 FILLER            PIC X(13) VALUE '8ARBM     444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBMD    444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBME    444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBMEH   444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBMH    444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBMHD   444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBMI    444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBMID   444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBMIH   444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBMIHD  444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBMIR   444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBMIRD  444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBMIRH  444'.
110100     05 FILLER            PIC X(13) VALUE '8ARBMIRHD 444'.

131198     05 FILLER            PIC X(13) VALUE 'ARBSF     462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSFD    462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSFE    462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSFEH   462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSFH    462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSFHD   462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSFI    462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSFID   462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSFIH   462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSFIHD  462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSFIR   462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSFIRD  462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSFIRH  462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSFIRHD 462'.

131198     05 FILLER            PIC X(13) VALUE 'ARBSM     462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSMD    462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSME    462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSMEH   462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSMH    462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSMHD   462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSMI    462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSMID   462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSMIH   462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSMIHD  462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSMIR   462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSMIRD  462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSMIRH  462'.
131198     05 FILLER            PIC X(13) VALUE 'ARBSMIRHD 462'.

011295     05 FILLER            PIC X(13) VALUE 'ARKF      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKFD     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKFE     445'.
011295     05 FILLER            PIC X(13) VALUE 'ARKFEH    445'.
011295     05 FILLER            PIC X(13) VALUE 'ARKFH     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKFHD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKFI     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKFID    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKFIH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKFIHD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKFIR    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKFIRD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKFIRH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKFIRHD  446'.

011295     05 FILLER            PIC X(13) VALUE 'ARKM      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKMD     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKME     445'.
011295     05 FILLER            PIC X(13) VALUE 'ARKMEH    445'.
011295     05 FILLER            PIC X(13) VALUE 'ARKMH     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKMHD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKMI     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKMID    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKMIH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKMIHD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKMIR    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKMIRD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKMIRH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKMIRHD  446'.

011295     05 FILLER            PIC X(13) VALUE 'ARTF      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFD     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFE     445'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFEH    445'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFH     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFHD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFI     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFID    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFIH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFIHD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFIR    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFIRD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFIRH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFIRHD  446'.

011295     05 FILLER            PIC X(13) VALUE 'ARTM      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMD     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTME     445'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMEH    445'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMH     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMHD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMI     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMID    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMIH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMIHD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMIR    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMIRD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMIRH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMIRHD  446'.

011295     05 FILLER            PIC X(13) VALUE 'ARTFM     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFMD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFME    445'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFMEH   445'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFMH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFMHD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFMI    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFMID   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFMIH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFMIHD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFMIR   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFMIRD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFMIRH  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTFMIRHD 446'.

011295     05 FILLER            PIC X(13) VALUE 'ARTMF     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMFD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMFE    445'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMFEH   445'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMFH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMFHD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMFI    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMFID   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMFIH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMFIHD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMFIR   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMFIRD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMFIRH  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARTMFIRHD 446'.

011295     05 FILLER            PIC X(13) VALUE 'ARKTF     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFE    456'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFEH   456'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFHD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFI    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFID   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFIH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFIHD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFIR   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFIRD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFIRH  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFIRHD 446'.

011295     05 FILLER            PIC X(13) VALUE 'ARKTM     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTME    445'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMEH   445'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMHD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMI    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMID   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMIH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMIHD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMIR   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMIRD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMIRH  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMIRHD 446'.

011295     05 FILLER            PIC X(13) VALUE 'ARKTFM    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFMD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFME   445'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFMEH  445'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFMH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFMHD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFMI   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFMID  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFMIH  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFMIHD 446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFMIR  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFMIRD 446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFMIRH 446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTFMIRHD446'.

011295     05 FILLER            PIC X(13) VALUE 'ARKTMF    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMFD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMFE   445'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMFEH  445'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMFH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMFHD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMFI   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMFID  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMFIH  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMFIHD 446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMFIR  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMFIRD 446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMFIRH 446'.
011295     05 FILLER            PIC X(13) VALUE 'ARKTMFIRHD446'.

011295     05 FILLER            PIC X(13) VALUE 'ARPF      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFD     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFE     445'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFEH    445'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFH     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFHD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFI     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFID    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFIH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFIHD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFIR    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFIRD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFIRH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFIRHD  446'.

011295     05 FILLER            PIC X(13) VALUE 'ARPM      446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMD     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPME     445'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMEH    445'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMH     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMHD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMI     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMID    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMIH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMIHD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMIR    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMIRD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMIRH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMIRHD  446'.

011295     05 FILLER            PIC X(13) VALUE 'ARPFM     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFMD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFME    445'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFMEH   445'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFMH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFMHD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFMI    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFMID   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFMIH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFMIHD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFMIR   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFMIRD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFMIRH  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPFMIRHD 446'.

011295     05 FILLER            PIC X(13) VALUE 'ARPMF     446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMFD    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMFE    445'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMFEH   445'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMFH    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMFHD   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMFI    446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMFID   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMFIH   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMFIHD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMFIR   446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMFIRD  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMFIRH  446'.
011295     05 FILLER            PIC X(13) VALUE 'ARPMFIRHD 446'.

011295     05 FILLER            PIC X(13) VALUE '1KSF      360'.
011295     05 FILLER            PIC X(13) VALUE '1KSFE     360'.
011295     05 FILLER            PIC X(13) VALUE '1KSFI     360'.
011295     05 FILLER            PIC X(13) VALUE '1KSFIR    360'.
011295     05 FILLER            PIC X(13) VALUE '1KSM      360'.
011295     05 FILLER            PIC X(13) VALUE '1KSME     360'.
011295     05 FILLER            PIC X(13) VALUE '1KSMI     360'.
011295     05 FILLER            PIC X(13) VALUE '1KSMIR    360'.

011295     05 FILLER            PIC X(13) VALUE '2KS150F   360'.
011295     05 FILLER            PIC X(13) VALUE '2KS150FE  360'.
011295     05 FILLER            PIC X(13) VALUE '2KS150FI  360'.
011295     05 FILLER            PIC X(13) VALUE '2KS150FIR 360'.
011295     05 FILLER            PIC X(13) VALUE '2KS200F   360'.
011295     05 FILLER            PIC X(13) VALUE '2KS200FE  360'.
011295     05 FILLER            PIC X(13) VALUE '2KS200FI  360'.
011295     05 FILLER            PIC X(13) VALUE '2KS200FIR 360'.

011295     05 FILLER            PIC X(13) VALUE '2KS150M   360'.
011295     05 FILLER            PIC X(13) VALUE '2KS150ME  360'.
011295     05 FILLER            PIC X(13) VALUE '2KS150MI  360'.
011295     05 FILLER            PIC X(13) VALUE '2KS150MIR 360'.
011295     05 FILLER            PIC X(13) VALUE '2KS200M   360'.
011295     05 FILLER            PIC X(13) VALUE '2KS200ME  360'.
011295     05 FILLER            PIC X(13) VALUE '2KS200MI  360'.
011295     05 FILLER            PIC X(13) VALUE '2KS200MIR 360'.

011295     05 FILLER            PIC X(13) VALUE '3KSF      360'.
011295     05 FILLER            PIC X(13) VALUE '3KSM      360'.
011295     05 FILLER            PIC X(13) VALUE '3KSFI     360'.
011295     05 FILLER            PIC X(13) VALUE '3KSMI     360'.
011295     05 FILLER            PIC X(13) VALUE '3KSFIR    360'.
011295     05 FILLER            PIC X(13) VALUE '3KSMIR    360'.

260597     05 FILLER            PIC X(13) VALUE '1K60M     271'.
260597     05 FILLER            PIC X(13) VALUE '1K60F     271'.
260597     05 FILLER            PIC X(13) VALUE '1K60MD    271'.
260597     05 FILLER            PIC X(13) VALUE '1K60FD    271'.
260597     05 FILLER            PIC X(13) VALUE '1K60MI    271'.
260597     05 FILLER            PIC X(13) VALUE '1K60FI    271'.
260597     05 FILLER            PIC X(13) VALUE '1K60MID   271'.
260597     05 FILLER            PIC X(13) VALUE '1K60FID   271'.
260597     05 FILLER            PIC X(13) VALUE '1K60MIR   271'.
260597     05 FILLER            PIC X(13) VALUE '1K60FIR   271'.
260597     05 FILLER            PIC X(13) VALUE '1K60MIRD  271'.
260597     05 FILLER            PIC X(13) VALUE '1K60FIRD  271'.
      *
241197     05 FILLER            PIC X(13) VALUE '8FLGF     628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGFD    628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGFI    628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGFID   628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGFIR   628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGFIRD  628'.
      *
241197     05 FILLER            PIC X(13) VALUE '8FLGTF    628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGTFD   628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGTFI   628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGTFID  628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGTFIR  628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGTFIRD 628'.
      *
241197     05 FILLER            PIC X(13) VALUE '8FLGM     628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGMD    628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGMI    628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGMID   628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGMIR   628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGMIRD  628'.
      *
241197     05 FILLER            PIC X(13) VALUE '8FLGTM    628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGTMD   628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGTMI   628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGTMID  628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGTMIR  628'.
241197     05 FILLER            PIC X(13) VALUE '8FLGTMIRD 628'.

221299*>>>>>
           05 FILLER            PIC X(13) VALUE '8FLGRF    629'.
           05 FILLER            PIC X(13) VALUE '8FLGRFD   629'.
           05 FILLER            PIC X(13) VALUE '8FLGRFI   629'.
           05 FILLER            PIC X(13) VALUE '8FLGRFID  629'.
           05 FILLER            PIC X(13) VALUE '8FLGRFIR  629'.
           05 FILLER            PIC X(13) VALUE '8FLGRFIRD 629'.
      *
           05 FILLER            PIC X(13) VALUE '8FLGRM    629'.
           05 FILLER            PIC X(13) VALUE '8FLGRMD   629'.
           05 FILLER            PIC X(13) VALUE '8FLGRMI   629'.
           05 FILLER            PIC X(13) VALUE '8FLGRMID  629'.
           05 FILLER            PIC X(13) VALUE '8FLGRMIR  629'.
           05 FILLER            PIC X(13) VALUE '8FLGRMIRD 629'.
      *
           05 FILLER            PIC X(13) VALUE '1RGM      486'.
           05 FILLER            PIC X(13) VALUE '1RGF      486'.
           05 FILLER            PIC X(13) VALUE '1RGMZ     486'.
           05 FILLER            PIC X(13) VALUE '1RGFZ     486'.
      *
           05 FILLER            PIC X(13) VALUE '2RGM      482'.
           05 FILLER            PIC X(13) VALUE '2RGF      482'.
           05 FILLER            PIC X(13) VALUE '2RGMZ     482'.
           05 FILLER            PIC X(13) VALUE '2RGFZ     482'.
      *
           05 FILLER            PIC X(13) VALUE '3RGM      488'.
           05 FILLER            PIC X(13) VALUE '3RGF      488'.
           05 FILLER            PIC X(13) VALUE '3RGMZ     488'.
           05 FILLER            PIC X(13) VALUE '3RGFZ     488'.
      *
           05 FILLER            PIC X(13) VALUE '1PKF      491'.
           05 FILLER            PIC X(13) VALUE '1PKFI     491'.
           05 FILLER            PIC X(13) VALUE '1PKFIR    491'.
           05 FILLER            PIC X(13) VALUE '1PKM      491'.
           05 FILLER            PIC X(13) VALUE '1PKMI     491'.
           05 FILLER            PIC X(13) VALUE '1PKMIR    491'.
      *
           05 FILLER            PIC X(13) VALUE '1PKBF     491'.
           05 FILLER            PIC X(13) VALUE '1PKBFI    491'.
           05 FILLER            PIC X(13) VALUE '1PKBFIR   491'.
           05 FILLER            PIC X(13) VALUE '1PKBM     491'.
           05 FILLER            PIC X(13) VALUE '1PKBMI    491'.
           05 FILLER            PIC X(13) VALUE '1PKBMIR   491'.
      *
           05 FILLER            PIC X(13) VALUE '1PKTF     491'.
           05 FILLER            PIC X(13) VALUE '1PKTFI    491'.
           05 FILLER            PIC X(13) VALUE '1PKTFIR   491'.
           05 FILLER            PIC X(13) VALUE '1PKTM     491'.
           05 FILLER            PIC X(13) VALUE '1PKTMI    491'.
           05 FILLER            PIC X(13) VALUE '1PKTMIR   491'.
161002*<<<<<
010604*>>>>>
           05 FILLER            PIC X(13) VALUE 'SKF       438'.
           05 FILLER            PIC X(13) VALUE 'SKFD      438'.
           05 FILLER            PIC X(13) VALUE 'SKFE      438'.
           05 FILLER            PIC X(13) VALUE 'SKM       438'.
           05 FILLER            PIC X(13) VALUE 'SKMD      438'.
           05 FILLER            PIC X(13) VALUE 'SKME      438'.
010604*<<<<<
           05 FILLER            PIC X(13) VALUE 'XXXXXXXXXXXXX'.
       01  TARTAB-R REDEFINES TARTAB.
010604*    05 T-FELD            OCCURS 2409.
010604     05 T-FELD            OCCURS 2415.
              10 T-TARIF        PIC X(10).
              10 T-SPARTE       PIC 9.
              10 T-PRODSCHL     PIC 99.
